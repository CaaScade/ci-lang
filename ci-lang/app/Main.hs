{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Import

import           GitHub.Archive
import           Koki.CI.App
import           Koki.CI.Docker.Types
import           Koki.CI.Lang
import           Koki.CI.Lang.Git     (prepareWorkspaceJob)
import           Koki.CI.Lang.GitHub  (doClone)
import           Koki.CI.Util
import           System.Directory     (createDirectoryIfMissing)
import           System.Environment   (lookupEnv)
import           System.Exit

getDockerURL :: IO Text
getDockerURL = do
  mDockerURL <- lookupEnv "DOCKER_HOST"
  let dockerURL = maybe "http://localhost:2375" pack mDockerURL
  return dockerURL

getWorkspaceDir :: IO FilePath
getWorkspaceDir = do
  mWorkspaceDir <- lookupEnv "CI_WORKSPACE_DIR"
  let workspaceDir = fromMaybe "/workspace" mWorkspaceDir
  createDirectoryIfMissing True workspaceDir
  return workspaceDir

getEnv :: String -> ExceptT String IO String
getEnv var = ExceptT $ do
  mVal <- lookupEnv var
  case mVal of Nothing  -> return (Left $ "no " <> var)
               Just val -> return (Right val)

getTarballRequest :: IO TarballRequest
getTarballRequest = do
  let action = do
        repoName <- getEnv "GIT_REPO_NAME"
        repoOwner <- getEnv "GIT_REPO_OWNER"
        revision <- getEnv "GIT_REVISION"
        token <- getEnv "GITHUB_OAUTH_TOKEN"
        return TarballRequest
          { _trRepoName = pack repoName
          , _trRepoOwner = pack repoOwner
          , _trRepoRef = pack revision
          , _trToken = pack token}
  result <- runExceptT action
  case result of
    Left e  -> printFlush e >> exitFailure
    Right x -> return x

main :: IO ()
main = do
  dockerURL <- getDockerURL
  workspaceDir <- getWorkspaceDir
  tarballRequest <- getTarballRequest
  let env = defaultAppEnv $ DockerBaseURL dockerURL
  result <- runApp env (script workspaceDir tarballRequest)
  case result of
    Left e   -> printFlush e >> exitFailure
    Right () -> return ()

script :: FilePath -> TarballRequest -> App ()
script workspaceDir tarballRequest = do
  untilDockerAvailable
  let prepare = prepareWorkspaceJob workspaceDir
  prepResult <- runContainerJob prepare
  when (prepResult /= ExitSuccess) $ throwSimple "prepare step exit failure"
  cloneResult <- liftIO $ doClone workspaceDir tarballRequest
  cloneDir <- case cloneResult of Left e -> throwSimple $ tshow e
                                  Right x -> return x
  jobs <- convertPipeline workspaceDir <$> getPipeline cloneDir
  pipelineResult <- runPipelineJobs jobs
  when (pipelineResult /= ExitSuccess) $ throwSimple "pipeline exit failure"

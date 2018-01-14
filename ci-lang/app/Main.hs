{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Import

import           Koki.CI.App
import           Koki.CI.Docker.Types
import           Koki.CI.Lang
import           Koki.CI.Lang.Git     (GitRepo (..), GitRevision (..), cloneJob,
                                       prepareWorkspaceJob)
import           Koki.CI.Util
import           System.Directory     (createDirectoryIfMissing)
import           System.Environment   (lookupEnv)
import           System.Exit

main :: IO ()
main = do
  putStrLn "hello from Haskell"
  dockerMain

getDockerURL :: IO Text
getDockerURL = do
  mDockerURL <- lookupEnv "DOCKER_HOST"
  let dockerURL = maybe "http://localhost:2375" pack mDockerURL
  putFlush $ "DOCKER_HOST=" <> dockerURL
  return dockerURL

getWorkspaceDir :: IO FilePath
getWorkspaceDir = do
  mWorkspaceDir <- lookupEnv "CI_WORKSPACE_DIR"
  let workspaceDir = fromMaybe "/home/kynan/workspace/ci-lang/ci-workspace" mWorkspaceDir
  createDirectoryIfMissing True workspaceDir
  return workspaceDir

getEnv :: String -> ExceptT String IO String
getEnv var = ExceptT $ do
  mVal <- lookupEnv var
  case mVal of Nothing -> return (Left $ "no " <> var)
               Just val -> return (Right val)

getGitInfo :: IO (GitRepo, GitRevision)
getGitInfo = do
  let action = do
        repoName <- getEnv "GIT_REPO_NAME"
        repoURL <- getEnv "GIT_REPO_URL"
        revision <- getEnv "GIT_REVISION"
        let repo = GitRepo {_repoName = pack repoName, _repoURL = pack repoURL}
        return (repo, GitRevision $ pack revision)
  result <- runExceptT action
  case result of
    Left e -> printFlush e >> exitFailure
    Right x -> return x

dockerMain :: IO ()
dockerMain = do
  dockerURL <- getDockerURL
  workspaceDir <- getWorkspaceDir
  (repo, revision) <- getGitInfo
  let env = printingAppEnv $ DockerBaseURL dockerURL
  result <- runApp env (script workspaceDir repo revision)
  case result of
    Left e   -> printFlush e >> exitFailure
    Right () -> return ()

script :: FilePath -> GitRepo -> GitRevision -> App ()
script workspaceDir gitRepo gitRevision = do
  untilDockerAvailable
  let prepare = prepareWorkspaceJob workspaceDir
      clone = cloneJob workspaceDir gitRepo gitRevision
  prepResult <- runContainerJob prepare
  when (prepResult /= ExitSuccess) $ throwSimple "prepare step exit failure"
  cloneResult <- runContainerJob clone
  when (cloneResult /= ExitSuccess) $ throwSimple "clone step exit failure"
  jobs <- convertPipeline workspaceDir <$> getPipeline workspaceDir (_repoName gitRepo)
  pipelineResult <- runPipelineJobs jobs
  when (pipelineResult /= ExitSuccess) $ throwSimple "pipeline exit failure"

printloop :: IO ()
printloop = do
  putFlush "loopy loop"
  threadDelay 2000000
  printloop

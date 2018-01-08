{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Import

import           Koki.CI.App
import           Koki.CI.Docker.Types
import           Koki.CI.Util
import           System.Directory     (createDirectoryIfMissing)
import           System.Environment   (lookupEnv)
import           System.FilePath      ((</>))

main :: IO ()
main = do
  putStrLn "hello from Haskell"
  dockerMain

dockerMain :: IO ()
dockerMain = do
  mDockerURL <- lookupEnv "DOCKER_HOST"
  let dockerURL = maybe "http://localhost:2375" pack mDockerURL
  putFlush $ "DOCKER_HOST=" <> dockerURL
  mWorkspaceDir <- lookupEnv "CI_WORKSPACE_DIR"
  let workspaceDir = fromMaybe "/home/kynan/workspace/ci-lang/ci-workspace" mWorkspaceDir
      env = printingAppEnv $ DockerBaseURL dockerURL
  createDirectoryIfMissing True workspaceDir
  printFlush =<< runApp env (script workspaceDir)

script :: FilePath -> App ()
script workspaceDir = do
  untilDockerAvailable
  printFlush =<< runContainerJob prepareWorkspaceJob
  printFlush =<< runContainerJob cloneJob
  printFlush =<< runContainerJob buildJob
  where
    prepareWorkspaceJob =
      ContainerJob
      { _cjImageName = "alpine"
      , _cjImageTag = ImageTag "latest"
      , _cjName = Nothing
      , _cjWorkspace =
          Workspace
          { _wHostDir = Directory workspaceDir
          , _wJobDir = Directory "/workspace"
          }
      , _cjCommands = [ "rm -rf *" ]
      , _cjTimeout = responseTimeoutDefault
      }
    cloneJob =
      ContainerJob
      { _cjImageName = "alpine/git"
      , _cjImageTag = ImageTag "latest"
      , _cjName = Nothing
      , _cjWorkspace =
          Workspace
          { _wHostDir = Directory workspaceDir
          , _wJobDir = Directory "/workspace"
          }
      , _cjCommands = [ "git clone https://github.com/koki/short.git"]
      , _cjTimeout = responseTimeoutMinutes 1
      }
    buildJob =
      ContainerJob
      { _cjImageName = "golang"
      , _cjImageTag = ImageTag "latest"
      , _cjName = Nothing
      , _cjWorkspace =
          Workspace
          { _wHostDir = Directory $ workspaceDir </> "short"
          , _wJobDir = Directory "/go/src/github.com/koki/short"
          }
      , _cjCommands = [ "./scripts/test.sh", "./scripts/build.sh" ]
      , _cjTimeout = responseTimeoutMinutes 5
      }

printloop :: IO ()
printloop = do
  putFlush "loopy loop"
  threadDelay 2000000
  printloop

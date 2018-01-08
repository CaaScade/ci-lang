{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Import

import           Koki.CI.App
import           Koki.CI.Docker.Types
import           Koki.CI.Util
import           System.Environment (lookupEnv)

main :: IO ()
main = do
  putStrLn "hello from Haskell"
  dockerMain

dockerMain :: IO ()
dockerMain = do
  mDockerURL <- lookupEnv "DOCKER_HOST"
  let dockerURL = maybe "http://localhost:2375" pack mDockerURL
  putFlush $ "DOCKER_HOST=" <> dockerURL
  env <- printingAppEnv $ DockerBaseURL dockerURL
  printFlush =<< runApp env script

script :: App ()
script = do
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
          { _wHostDir = Directory "/home/kynan/workspace/scratch/ci-workspace"
          , _wJobDir = Directory "/workspace"
          }
      , _cjCommands = [ "rm -rf *" ]
      }
    cloneJob =
      ContainerJob
      { _cjImageName = "alpine/git"
      , _cjImageTag = ImageTag "latest"
      , _cjName = Nothing
      , _cjWorkspace =
          Workspace
          { _wHostDir = Directory "/home/kynan/workspace/scratch/ci-workspace"
          , _wJobDir = Directory "/workspace"
          }
      , _cjCommands = [ "git clone https://github.com/koki/short.git"]
      }
    buildJob =
      ContainerJob
      { _cjImageName = "golang"
      , _cjImageTag = ImageTag "latest"
      , _cjName = Nothing
      , _cjWorkspace =
          Workspace
          { _wHostDir = Directory "/home/kynan/workspace/scratch/ci-workspace/short"
          , _wJobDir = Directory "/go/src/github.com/koki/short"
          }
      , _cjCommands = [ "./scripts/test.sh", "./scripts/build.sh" ]
      }

printloop :: IO ()
printloop = do
  putFlush "loopy loop"
  threadDelay 2000000
  printloop

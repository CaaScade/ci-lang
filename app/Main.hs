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
  printFlush =<< runContainerJob job
  where
    job =
      ContainerJob
      { _cjImageName = "ubuntu"
      , _cjImageTag = ImageTag "16.04"
      , _cjName = Nothing
      , _cjWorkspace =
          Workspace
          { _wHostDir = Directory "/home/kynan/workspace/scratch/ci-workspace"
          , _wJobDir = Directory "/workspace"
          }
      , _cjCommands = [ "touch /workspace/a", "touch /workspace/b" ]
      }

printloop :: IO ()
printloop = do
  putFlush "loopy loop"
  threadDelay 2000000
  printloop

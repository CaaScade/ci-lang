{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Import

import           Koki.CI.App
import           Koki.CI.Docker
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
  printFlush =<< runAppM env script

script :: AppM ()
script = do
  putFlush "check if available"
  liftDocker untilDockerAvailable
  putFlush "pull container"
  liftDocker pullNginxContainer
  putFlush "create container"
  cid <- liftDocker $ untilDockerSucceeds createNginxContainer
  putFlush $ "start " <> tshow cid
  liftDocker $ startDockerContainer cid
  putFlush $ "wait for " <> tshow cid
  code <- liftDocker $ waitDockerContainer cid
  putFlush $ tshow code

printloop :: IO ()
printloop = do
  putFlush "loopy loop"
  threadDelay 2000000
  printloop

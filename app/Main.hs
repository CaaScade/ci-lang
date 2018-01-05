{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Import

import Koki.CI.Docker
import System.Environment (lookupEnv)
import GHC.IO.Handle (hFlush)

main :: IO ()
main = do
  putStrLn "hello from Haskell"
  dockerMain

dockerMain :: IO ()
dockerMain = do
  mDockerURL <- lookupEnv "DOCKER_HOST"
  let dockerURL = maybe "http://localhost:2375" pack mDockerURL
  putFlush $ "DOCKER_HOST=" <> dockerURL
  runEDockerT dockerURL script

script :: EDockerT IO ()
script = do
  putFlush "check if available"
  untilDockerAvailable
  putFlush "pull container"
  pullNginxContainer
  putFlush "create container"
  cid <- untilDockerSucceeds createNginxContainer
  putFlush $ "start " <> tshow cid
  startDockerContainer cid
  putFlush $ "wait for " <> tshow cid
  code <- waitDockerContainer cid
  putFlush $ tshow code

printloop :: IO ()
printloop = do
  putFlush "loopy loop"
  threadDelay 2000000
  printloop

putFlush :: MonadIO m => Text -> m ()
putFlush msg = liftIO (putStrLn msg >> hFlush stdout)

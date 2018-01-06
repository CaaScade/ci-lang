{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Koki.CI.App
  ( module Koki.CI.App
  , module Koki.CI.App.Types
  ) where

import           Import

import qualified Docker.Client       as D
import           Koki.CI.App.Types
import           Koki.CI.Util
import           Network.HTTP.Client

printRequest :: Request -> IO Request
printRequest request = do
  printFlush request
  return request

printResponse :: Response BodyReader -> IO (Response BodyReader)
printResponse response_ = do
  reconsumed <- newIORef False
  let wholeResponse = fmap f response_
  response <- sequence wholeResponse
  printFlush response
  let returnOnce body = do
        wasReconsumed <- readIORef reconsumed
        if wasReconsumed
          then return mempty
          else writeIORef reconsumed True >> return body
  return $ fmap returnOnce response
  where
    f bodyreader = do
      chunks <- brConsume bodyreader
      return $ mconcat chunks

printingAppEnv :: DockerBaseURL -> IO AppEnv
printingAppEnv baseURL = do
  manager <-
    newManager
      defaultManagerSettings
      { managerModifyRequest = printRequest
      , managerModifyResponse = printResponse
      }
  return
    AppEnv
    { _aeHttpManager = manager
    , _aeHttpHandler = D.httpHandler manager
    , _aeDockerOpts = dockerOptsForBaseURL baseURL
    }

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Koki.CI.App
  ( module Koki.CI.App
  , module Koki.CI.App.Types
  ) where

import           Import

import           Koki.CI.App.Types
import           Koki.CI.Docker.Types
import           Koki.CI.Lang
import qualified Koki.CI.Lang.Git     as G
import           Koki.CI.Util
import           Network.HTTP.Client
import           System.Exit

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

printingAppEnv :: DockerBaseURL -> AppEnv
printingAppEnv baseURL =
  AppEnv {_aeHttpManagerSettings = managerSettings, _aeDockerBaseURL = baseURL}
  where
    managerSettings =
      defaultManagerSettings
      { managerModifyRequest = printRequest
      , managerModifyResponse = printResponse
      }

getPipeline :: FilePath -> Text -> App Pipeline
getPipeline workspaceDir repoName = do
  result <- G.getPipeline workspaceDir repoName
  case result of
    Left e         -> throwLang e
    Right pipeline -> return pipeline

runPipelineJobs :: [ContainerJob] -> App ExitCode
runPipelineJobs [] = return ExitSuccess
runPipelineJobs (job:jobs) = do
  code <- runContainerJob job
  case code of ExitSuccess -> runPipelineJobs jobs
               failure     -> return failure


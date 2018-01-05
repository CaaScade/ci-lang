{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Koki.CI.Docker where

import           Import

import           Conduit
import           Docker.Client       hiding (stdout)
import           GHC.IO.Handle       (hFlush)
import           Network.HTTP.Client
import           System.Exit         (ExitCode)

type EDockerT m a = ExceptT DockerError (DockerT m) a

runEDockerT :: (MonadMask m, MonadIO m) => URL -> EDockerT m a -> m a
runEDockerT baseURL action = do
  mngr <- liftIO $ newManager dockerManagerSettings
  let h = httpHandler mngr
  let opts = defaultClientOpts { baseUrl = baseURL }
  r <- runDockerT (opts, h) . runExceptT $ action
  case r of Left e  -> error $ show e
            Right x -> return x

createNginxContainer :: EDockerT IO ContainerID
createNginxContainer = ExceptT $ createContainer opts (Just "myNginxContainer")
  where
    pb = PortBinding 80 TCP [HostPort "0.0.0.0" 8000]
    opts = addPortBinding pb $ defaultCreateOpts "nginx:latest"

pullNginxContainer :: EDockerT IO ()
pullNginxContainer = ExceptT $ pullImage "nginx" "latest" printC

startDockerContainer :: ContainerID -> EDockerT IO ()
startDockerContainer = ExceptT . startContainer defaultStartOpts

stopDockerContainer :: ContainerID -> EDockerT IO ()
stopDockerContainer = ExceptT . stopContainer DefaultTimeout

waitDockerContainer :: ContainerID -> EDockerT IO ExitCode
waitDockerContainer = ExceptT . waitContainer

untilDockerAvailable :: EDockerT IO ()
untilDockerAvailable =
  untilDockerSucceeds (print =<< getDockerVersion')
  where getDockerVersion' = ExceptT getDockerVersion

untilDockerSucceeds :: EDockerT IO a -> EDockerT IO a
untilDockerSucceeds = ExceptT . fmap Right . untilDockerSucceeds_

untilDockerSucceeds_ :: EDockerT IO a -> DockerT IO a
untilDockerSucceeds_ action = do
  r <- runExceptT action
  case r of
    Left e -> do
      print e
      liftIO $ threadDelay 4000000
      liftIO $ hFlush stdout
      untilDockerSucceeds_ action
    Right v -> return v

dockerManagerSettings :: ManagerSettings
dockerManagerSettings =
  defaultManagerSettings { managerModifyRequest = logRequest
                         , managerModifyResponse = logResponse
                         }
  where logRequest request = do
          print request
          hFlush stdout
          return request
        logResponse response_ = do
          reconsumed <- newIORef False
          let wholeResponse = fmap f response_
          response <- sequence wholeResponse
          print response
          hFlush stdout
          let returnOnce body = do
                wasReconsumed <- readIORef reconsumed
                if wasReconsumed then return mempty
                else writeIORef reconsumed True >> return body
          return $ fmap returnOnce response
        f bodyreader = do
          chunks <- brConsume bodyreader
          return $ mconcat chunks

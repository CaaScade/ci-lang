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

data ContainerJob = ContainerJob
  { _cjContainerName :: Text
  , _cjContainerTag :: Tag
  , _cjWorkspaceBind :: Bind
  }

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

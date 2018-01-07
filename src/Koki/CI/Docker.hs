{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Koki.CI.Docker where

import           Import

import           Conduit
import           Docker.Client       hiding (stdout)
import           GHC.IO.Handle       (hFlush)
import           Network.HTTP.Client
import           System.Exit         (ExitCode)
import Koki.CI.Docker.Types

type EDockerT m a = ExceptT DockerError (DockerT m) a

containerJobCreateOpts :: ContainerJob -> CreateOpts
containerJobCreateOpts job =
  defaultCreateOpts . unImageTaggedName $ containerJobImageTaggedName job

createJobContainer :: ContainerJob -> EDockerT IO ContainerID
createJobContainer job@ContainerJob{..} =
  ExceptT $ createContainer (containerJobCreateOpts job) _cjName

pullJobImage :: ContainerJob -> EDockerT IO ()
pullJobImage ContainerJob{..} =
  ExceptT $ pullImage _cjImageName tag printC -- TODO: something other than printC
  where tag = unImageTag _cjImageTag

runContainerJob :: ContainerJob -> EDockerT IO ExitCode
runContainerJob job = do
  pullJobImage job
  cid <- createJobContainer job
  ExceptT $ startContainer defaultStartOpts cid
  ExceptT $ waitContainer cid

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

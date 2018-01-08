{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Koki.CI.Docker where

import           Import

import           Conduit
import qualified Data.Text            as T
import           Docker.Client        hiding (stdout)
import           GHC.IO.Handle        (hFlush)
import           Koki.CI.Docker.Types
import           System.Exit          (ExitCode)

type EDockerT m a = ExceptT DockerError (DockerT m) a

containerJobCreateOpts :: ContainerJob -> CreateOpts
containerJobCreateOpts job@ContainerJob {..} =
  setContainerConfig . bindWorkspace $ defaultCreateOpts imageName
  where
    bindWorkspace = addBind (workspaceBindMount _cjWorkspace)
    setContainerConfig opts =
      opts
      { containerConfig =
          cc
          { cmd = jobCommands _cjCommands
          , workingDir = Just jobDir
          , entrypoint = Entrypoint [""]
          }
      }
      where
        cc = containerConfig opts
    imageName = unImageTaggedName $ containerJobImageTaggedName job
    jobDir = unDirectory $ _wJobDir _cjWorkspace

workspaceBindMount :: Workspace -> Bind
workspaceBindMount Workspace {..} =
  Bind
  { hostSrc = T.pack $ unDirectory _wHostDir
  , containerDest = T.pack $ unDirectory _wJobDir
  , volumePermission = Nothing
  }

jobCommands :: [Text] -> [Text]
jobCommands script = ["sh", "-c", T.unlines ("set -e":script)]

createJobContainer :: ContainerJob -> EDockerT IO ContainerID
createJobContainer job@ContainerJob{..} =
  ExceptT $ createContainer (containerJobCreateOpts job) _cjName

pullJobImage :: ContainerJob -> EDockerT IO ()
pullJobImage ContainerJob{..} =
  ExceptT $ pullImage _cjImageName tag printC -- TODO: something other than printC
  where tag = unImageTag _cjImageTag

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

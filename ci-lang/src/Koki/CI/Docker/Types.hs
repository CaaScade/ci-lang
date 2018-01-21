{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Koki.CI.Docker.Types
  ( module Koki.CI.Docker.Types
  , ExitCode(..)
  , ResponseTimeout
  , responseTimeoutDefault
  , responseTimeoutNone
  , responseTimeoutMicro
  ) where

import           Import

import           Network.HTTP.Client (ResponseTimeout, responseTimeoutDefault,
                                      responseTimeoutMicro, responseTimeoutNone)
import           System.Exit         (ExitCode (..))

newtype ImageTaggedName = ImageTaggedName { unImageTaggedName :: Text } deriving (Show, Eq)
newtype ImageTag = ImageTag { unImageTag :: Text } deriving (Show, Eq)

data ContainerJob = ContainerJob
  { _cjImageName :: Text -- ^ the name of the image
  , _cjImageTag  :: ImageTag -- ^ the tag to pull
  , _cjName      :: Maybe Text -- ^ the name of the container
  , _cjWorkspace :: Workspace -- ^ the container's workspace
  , _cjCommands  :: [Text] -- ^ commands to run in the container
  , _cjTimeout :: ResponseTimeout -- ^ how long to wait for the job to finish
  } deriving (Show, Eq)

{- |
A `Workspace` defines the inputs and outputs of a Job.

For the moment, we assume that every Job can use the same workspace directory.

TODO: Don't assume single workspace dir.
      1. Job expects specific directories as input.
      2. Job specifies directories as output.
      3. Host mounts input directories in the right spots.
      4a. Host mounts output directories in the right spots.
      4b. For outputs that nest inside an input (esp if they modify the input),
          use layered filesystem to extract modified/new directory without affecting input.
-}
data Workspace = Workspace
  { _wHostDir :: Directory
  , _wJobDir  :: Directory } deriving (Show, Eq)
newtype Directory = Directory { unDirectory :: FilePath } deriving (Show, Eq)

-- | Typeclass for running jobs in Docker.
class Monad m => DockerJobClient m where
  -- | Returns 'ExitCode' rather than throwing error because the job still ran.
  -- The 'ExitCode' is a legitimate result that we care about.
  runContainerJob :: ContainerJob -> m ExitCode
  untilDockerAvailable :: m ()
  -- TODO: Add members for setting up inputs and retrieving outputs

containerJobImageTaggedName :: ContainerJob -> ImageTaggedName
containerJobImageTaggedName ContainerJob{..} = ImageTaggedName $ _cjImageName <> ":" <> unImageTag _cjImageTag

responseTimeoutMinutes :: Int -> ResponseTimeout
responseTimeoutMinutes x = responseTimeoutSeconds (60 * x)

responseTimeoutSeconds :: Int -> ResponseTimeout
responseTimeoutSeconds x = responseTimeoutMicro (1000000 * x)

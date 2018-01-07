{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Koki.CI.Docker.Types
  ( module Koki.CI.Docker.Types
  , ExitCode(..)
  ) where

import           Import

import           System.Exit   (ExitCode (..))

newtype ImageTaggedName = ImageTaggedName { unImageTaggedName :: Text } deriving (Show, Eq)
newtype ImageTag = ImageTag { unImageTag :: Text } deriving (Show, Eq)

-- TODO: Implementation-agnostic types here.
data ContainerJob = ContainerJob
  { _cjImageName :: Text
  , _cjImageTag  :: ImageTag
  , _cjName      :: Maybe Text
  , _cjWorkspace :: Workspace
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
  , _wJobDir :: Directory } deriving (Show, Eq)
newtype Directory = Directory { unDirectory :: FilePath } deriving (Show, Eq)

-- | Typeclass for running jobs in Docker.
class Monad m => DockerJobClient m where
  runContainerJob :: ContainerJob -> m ExitCode
  untilDockerAvailable :: m ()
  -- TODO: Add members for setting up inputs and retrieving outputs

containerJobImageTaggedName :: ContainerJob -> ImageTaggedName
containerJobImageTaggedName ContainerJob{..} = ImageTaggedName $ _cjImageName <> ":" <> unImageTag _cjImageTag

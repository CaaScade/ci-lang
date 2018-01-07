{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Koki.CI.Docker.Types
  ( module Koki.CI.Docker.Types
  , ExitCode(..)
  ) where

import           Import

import           Docker.Client
import           System.Exit   (ExitCode (..))

newtype ImageTaggedName = ImageTaggedName { unImageTaggedName :: Text } deriving (Show, Eq)
newtype ImageTag = ImageTag { unImageTag :: Text } deriving (Show, Eq)


-- TODO: Implementation-agnostic types here.
data ContainerJob = ContainerJob
  { _cjImageName     :: Text
  , _cjImageTag      :: ImageTag
  , _cjName          :: Maybe Text
  -- TODO: , _cjWorkspaceBind :: Bind
  } deriving (Show, Eq)

-- | Typeclass for running jobs in Docker.
class Monad m => DockerJobClient m where
  runContainerJob :: ContainerJob -> m ExitCode
  untilDockerAvailable :: m ()
  -- TODO: Add members for setting up inputs and retrieving outputs

containerJobImageTaggedName :: ContainerJob -> ImageTaggedName
containerJobImageTaggedName ContainerJob{..} = ImageTaggedName $ _cjImageName <> ":" <> unImageTag _cjImageTag

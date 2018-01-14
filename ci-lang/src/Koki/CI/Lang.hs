{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}

module Koki.CI.Lang
  ( module Koki.CI.Lang
  , ParseException(..)
  ) where

import           Import

import qualified Data.Aeson           as AE
import           Data.Maybe           (maybe)
import           Data.Yaml            (ParseException (..))

import           Koki.CI.Docker.Types

{- |
This module contains the CI pipeline language implementation.

In its first iteration, the CI language allows us to define a sequence of containerized steps, e.g.

- image: alpine
  tag: latest
  name: prepare-workspace
  hostDir: /workspace
  jobDir: /workspace
  commands:
  - "rm -rf *"
- image: alpine/git
  tag: latest
  name: git-clone
  hostDir: /workspace
  jobDir: /workspace
  commands:
  - "git clone https://github.com/koki/short.git"
  timeout: 60
- image: golang
  tag: latest
  name: test-and-build
  hostDir: /workspace/short
  jobDir: /go/src/github.com/koki/short
  commands:
  - "./scripts/test.sh"
  - "./scripts/build.sh"
  timeout: 300
-}

type Pipeline = [Pipe]

data Pipe = Pipe
  { image    :: Text
  , tag      :: Text
  , name     :: Maybe Text
  , hostDir  :: Text
  , jobDir   :: Text
  , commands :: [Text]
  , timeout  :: Maybe Int
  } deriving (Generic, Show)

instance AE.ToJSON Pipe where
  toEncoding = AE.genericToEncoding AE.defaultOptions
instance AE.FromJSON Pipe

convertPipe :: FilePath -> Pipe -> ContainerJob
convertPipe workspaceDir Pipe {..} =
  ContainerJob
  { _cjImageName = image
  , _cjImageTag = ImageTag tag
  , _cjName = name
  , _cjWorkspace =
      Workspace
      { _wHostDir = Directory $ workspaceDir </> unpack hostDir
      , _wJobDir = Directory $ workspaceDir </> unpack jobDir
      }
  , _cjCommands = commands
  , _cjTimeout = maybe responseTimeoutDefault responseTimeoutSeconds timeout
  }

convertPipeline :: FilePath -> Pipeline -> [ContainerJob]
convertPipeline workspaceDir = fmap $ convertPipe workspaceDir


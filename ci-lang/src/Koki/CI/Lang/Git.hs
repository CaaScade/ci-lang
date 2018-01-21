{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Koki.CI.Lang.Git where

import           Import

import qualified Data.Yaml            as Y

import           Koki.CI.Docker.Types
import           Koki.CI.Lang

data GitRepo = GitRepo
  { _repoName :: Text
  , _repoURL  :: Text
  } deriving (Show, Eq)
newtype GitRevision = GitRevision { unGitRevision :: Text } deriving (Show, Eq)

prepareWorkspaceJob :: FilePath -> ContainerJob
prepareWorkspaceJob workspaceDir =
  ContainerJob
  { _cjImageName = "alpine"
  , _cjImageTag = ImageTag "latest"
  , _cjName = Nothing
  , _cjWorkspace =
      Workspace
      {_wHostDir = Directory workspaceDir, _wJobDir = Directory "/workspace"}
  , _cjCommands = ["rm -rf *"]
  , _cjTimeout = responseTimeoutDefault
  }

cloneJob :: FilePath -> GitRepo -> GitRevision -> ContainerJob
cloneJob workspaceDir GitRepo {..} revision =
  ContainerJob
  { _cjImageName = "alpine/git"
  , _cjImageTag = ImageTag "latest"
  , _cjName = Nothing
  , _cjWorkspace =
      Workspace
      {_wHostDir = Directory workspaceDir, _wJobDir = Directory "/workspace"}
  , _cjCommands =
      [ "git clone " <> _repoURL
      , "cd " <> _repoName
      , "git checkout " <> unGitRevision revision
      ]
  , _cjTimeout = responseTimeoutDefault
  }

pipelineFileDefaultName :: FilePath
pipelineFileDefaultName = "koki.yaml"

getPipeline :: (MonadIO m) => FilePath -> m (Either ParseException Pipeline)
getPipeline cloneDir = do
  let pipelineFile = cloneDir </> pipelineFileDefaultName
  liftIO $ Y.decodeFileEither pipelineFile

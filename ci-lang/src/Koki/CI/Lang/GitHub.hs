{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Koki.CI.Lang.GitHub where

import Import

import System.Directory
import GitHub.Archive

data GitHubError
  = GitHubTarballError TarballError
  | GitHubTarballRequestError TarballRequestError
  | GitHubError String
  deriving (Show)

doClone :: FilePath -> TarballRequest -> IO (Either GitHubError FilePath)
doClone workspaceDir request = runExceptT $ do
  ExceptT . fmap (first GitHubTarballRequestError) $ saveTarball request targz
  ExceptT . fmap (first GitHubTarballError) $ extractTarball targz workspaceDir
  liftIO $ removeFile targz
  subdirs <- liftIO $ listDirectory workspaceDir
  case subdirs of
    [subdir] -> liftIO $ renameDirectory (workspaceDir </> subdir) cloneDir
    _ -> throwError . GitHubError $ "expected only one subdir: " <> show subdirs
  return cloneDir
  where targz = workspaceDir </> "tarball.tar.gz"
        cloneDir = workspaceDir </> unpack (_trRepoName request)

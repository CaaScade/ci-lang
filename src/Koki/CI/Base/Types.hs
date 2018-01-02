{-# LANGUAGE NoImplicitPrelude #-}

module Koki.CI.Base.Types where

import Import

{- TODO: Figure out how to generically handle Plugins and Properties.
   The answer probably is that these types need to be embedded in an AST.
   For POC, keeping the types as plain Haskell is fine.
-}


data Pipeline a
-- instance Monad Pipeline

data GitHubPullRequest
data GitHubMerge = GitHubMerge
  { _ghmBranch :: Text
  , _ghmRevision :: Text
  } deriving (Show, Eq)

data AccessRule
data User

data KubeSecret
data KubeConfigMap

-- class File file where

-- File types
data NormalFile
data Directory

class Secret secret where
  readKubeSecret :: KubeSecret -> Pipeline secret

-- Secret types
data SecretCertificate
data SecretUsernamePassword
data SecretKey

type Revision = Text
class CloneRepository repo where
  clone :: Secret -> repo -> Revision -> Pipeline Directory

-- CloneRepository types
data GitHubRepository
data BitbucketRepository
data GitRepository
data MercurialRepository

class PublishRepository repo where
  publish :: Secret -> NormalFile -> Pipeline ()

-- PublishRepository types
data DockerHub

-- class NetworkRequest

{-
TODO: Represent `Property`?
-}

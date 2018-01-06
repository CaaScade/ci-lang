{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Koki.CI.App.Types where

import           Import

import qualified Docker.Client       as D
import           Koki.CI.Docker
import           Network.HTTP.Client

newtype DockerBaseURL = DockerBaseURL Text deriving Show
defaultDockerBaseURL :: DockerBaseURL
defaultDockerBaseURL = DockerBaseURL "http://localhost:2375"

data AppEnv = AppEnv
  { _aeHttpManager :: Manager
  , _aeHttpHandler :: D.HttpHandler IO
  , _aeDockerOpts  :: D.DockerClientOpts
  }

data AppError
  = DockerError D.DockerError
  | SimpleError Text
  deriving (Show)

type AppM a = ReaderT AppEnv (ExceptT AppError IO) a

liftDocker :: forall a. EDockerT IO a -> AppM a
liftDocker action = do
  env <- ask
  let handler = _aeHttpHandler env
      opts = _aeDockerOpts env
      ranDocker :: IO (Either AppError a)
      ranDocker =
        fmap (first DockerError) . D.runDockerT (opts, handler) . runExceptT $
        action
  ReaderT . const . ExceptT $ ranDocker

dockerOptsForBaseURL :: DockerBaseURL -> D.DockerClientOpts
dockerOptsForBaseURL (DockerBaseURL baseURL) =
  D.defaultClientOpts { D.baseUrl = baseURL }

runAppM :: AppEnv -> AppM a -> IO (Either AppError a)
runAppM env action = runExceptT $ runReaderT action env

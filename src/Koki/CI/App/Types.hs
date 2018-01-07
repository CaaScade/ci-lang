{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Koki.CI.App.Types where

import           Import

import qualified Docker.Client        as D
import qualified Koki.CI.Docker       as KD
import           Koki.CI.Docker.Types
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

newtype App a = App
  { unApp :: ReaderT AppEnv (ExceptT AppError IO) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader AppEnv
             , MonadError AppError
             )

instance DockerJobClient App where

  runContainerJob :: ContainerJob -> App ExitCode
  runContainerJob job = liftDocker $ KD.runContainerJob job

  untilDockerAvailable :: App ()
  untilDockerAvailable = liftDocker KD.untilDockerAvailable

liftDocker :: forall a. KD.EDockerT IO a -> App a
liftDocker action = do
  env <- ask
  let handler = _aeHttpHandler env
      opts = _aeDockerOpts env
      ranDocker :: IO (Either AppError a)
      ranDocker =
        fmap (first DockerError) . D.runDockerT (opts, handler) . runExceptT $
        action
  App . ReaderT . const . ExceptT $ ranDocker

dockerOptsForBaseURL :: DockerBaseURL -> D.DockerClientOpts
dockerOptsForBaseURL (DockerBaseURL baseURL) =
  D.defaultClientOpts { D.baseUrl = baseURL }

runApp :: AppEnv -> App a -> IO (Either AppError a)
runApp env action = runExceptT $ runReaderT (unApp action) env

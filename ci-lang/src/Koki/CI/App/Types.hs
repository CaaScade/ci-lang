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
import           Koki.CI.Lang         (ParseException (..))
import           Network.HTTP.Client

newtype DockerBaseURL = DockerBaseURL Text deriving Show
defaultDockerBaseURL :: DockerBaseURL
defaultDockerBaseURL = DockerBaseURL "http://localhost:2375"

data AppEnv = AppEnv
  { _aeHttpManagerSettings :: ManagerSettings
  , _aeDockerBaseURL       :: DockerBaseURL
  }

data AppError
  = DockerError D.DockerError
  | LangError ParseException
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
  runContainerJob job = do
    liftDocker' $ KD.pullJobImage job
    cid <- liftDocker' $ KD.createJobContainer job
    liftDocker' $ KD.startDockerContainer cid
    liftDocker timeout $ KD.waitDockerContainer cid
    where
      timeout = _cjTimeout job
  untilDockerAvailable :: App ()
  untilDockerAvailable = liftDocker' KD.untilDockerAvailable

liftDocker' :: forall a. KD.EDockerT IO a -> App a
liftDocker' = liftDocker responseTimeoutDefault

liftDocker :: forall a. ResponseTimeout -> KD.EDockerT IO a -> App a
liftDocker timeout action = do
  env <- App ask
  let managerSettings = (_aeHttpManagerSettings env) { managerResponseTimeout = timeout }
  manager <- App . liftIO $ newManager managerSettings
  let handler = D.httpHandler manager
      opts = _aeDockerOpts env
      ranDocker :: IO (Either AppError a)
      ranDocker =
        fmap (first DockerError) . D.runDockerT (opts, handler) . runExceptT $
        action
  App . ReaderT . const . ExceptT $ ranDocker

dockerOptsForBaseURL :: DockerBaseURL -> D.DockerClientOpts
dockerOptsForBaseURL (DockerBaseURL baseURL) =
  D.defaultClientOpts { D.baseUrl = baseURL }

_aeDockerOpts :: AppEnv -> D.DockerClientOpts
_aeDockerOpts env =
  D.defaultClientOpts {D.baseUrl = coerce $ _aeDockerBaseURL env}

runApp :: AppEnv -> App a -> IO (Either AppError a)
runApp env action = runExceptT $ runReaderT (unApp action) env

throwSimple :: Text -> App a
throwSimple = App . throwError . SimpleError

throwLang :: ParseException -> App a
throwLang = App . throwError . LangError

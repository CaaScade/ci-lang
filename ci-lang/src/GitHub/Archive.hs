{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module GitHub.Archive where

import           Import

import           Data.Bifunctor
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Text.Lazy.Encoding as LTE
import           Network.Wreq

import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Check as Tar
import qualified Codec.Compression.GZip  as GZip

import           Koki.CI.Util

data TarballRequest = TarballRequest
  { _trRepoOwner :: Text
  , _trRepoName  :: Text
  , _trRepoRef   :: Text
  , _trToken     :: Text
  } deriving Eq

instance Show TarballRequest where
  -- Never show the token.
  show TarballRequest {..} =
    "TarballRequest " <> show _trRepoOwner <> " " <> show _trRepoName <> " \"*****\""

data TarballRequestError = TarballRequestError
  { _treStatus :: Status
  , _treBody   :: Text
  } deriving (Show, Eq)

buildTarballURL :: TarballRequest -> String
buildTarballURL TarballRequest {..} =
  unpack $ "https://api.github.com/repos/" <> _trRepoOwner <> "/" <> _trRepoName <>
  "/tarball/" <> _trRepoRef

getTarball :: TarballRequest -> IO (Response LByteString)
getTarball request@TarballRequest{..} = do
  let url = buildTarballURL request
  putFlush $ "downloading from " <> pack url
  getWith opts url
  where opts = defaults & auth ?~ oauth2Bearer (encodeUtf8 _trToken)

saveTarball :: TarballRequest -> FilePath -> IO (Either TarballRequestError ())
saveTarball request path = do
  response <- getTarball request
  case response ^. responseStatus . statusCode of
    200 -> Right <$> LBS.writeFile path (response ^. responseBody)
    _ ->
      return $
      Left
        TarballRequestError
        { _treStatus = response ^. responseStatus
        , _treBody = toStrict . LTE.decodeUtf8 $ response ^. responseBody
        }

data TarballError = TarballError String deriving Show

-- TODO: check for tarbomb
extractTarball :: FilePath -> FilePath -> IO (Either TarballError ())
extractTarball targz dir = do
  result <- try (Tar.unpack dir . Tar.read . GZip.decompress =<< LBS.readFile targz)
  case result of Left e -> return . Left . TarballError $ show (e :: Tar.FormatError)
                 Right () -> return . Right $ ()

{-# LANGUAGE NoImplicitPrelude #-}

module Koki.CI.Util where

import Import

import GHC.IO.Handle (hFlush)

putFlush :: MonadIO m => Text -> m ()
putFlush msg = liftIO (putStrLn msg >> hFlush stdout)

printFlush :: (Show a, MonadIO m) => a -> m ()
printFlush msg = liftIO (print msg >> hFlush stdout)

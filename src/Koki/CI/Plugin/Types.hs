{-# LANGUAGE NoImplicitPrelude #-}

module Koki.CI.Plugin.Types where

import Import

import Koki.CI.Base.Types

{- |
These types are for plugin authors. The language doesn't preclude passing around
values of these types in a pipeline, but we don't need users to know that.
-}

data DockerImage

-- TODO: Represent an arbitrary function type on 

{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( -- * Public API
    fetchAllWorkouts
  , fetchAllWorkoutsWithLog
  , fetchAllWorkoutsGrouped
  , fetchAllWorkoutsGroupedWithLog
  , fetchAllWorkoutsWithConfig
  , fetchAllWorkoutsWithConfigAndLog  -- Exportado para testes
    -- * Configuration
  , FetchConfig(..)
  , defaultConfig
    -- * HTTP Request Abstraction
  , HttpRequest
  , defaultHttpRequest
  ) where

import Lib.API
import Lib.Types
import Lib.HTTP
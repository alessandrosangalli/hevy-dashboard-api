{-# LANGUAGE OverloadedStrings #-}

module Lib.Types
  ( FetchConfig(..)
  , defaultConfig
  , HttpRequest
  ) where

import Network.HTTP.Client (Request, Manager, Response)
import Data.ByteString.Lazy (ByteString)

-- | Configuration for fetching workouts.
data FetchConfig = FetchConfig
  { fcPageSize :: Int      -- ^ Number of workouts per page
  , fcRetries :: Int       -- ^ Number of retries for HTTP requests
  , fcDelay :: Int         -- ^ Delay between retries (in microseconds)
  }

-- | Default configuration for fetching workouts.
defaultConfig :: FetchConfig
defaultConfig = FetchConfig
  { fcPageSize = 10
  , fcRetries = 3
  , fcDelay = 1000000
  }

-- | Type alias for an HTTP request function, injectable for testing.
type HttpRequest = Request -> Manager -> Int -> IO (Either String (Response ByteString))
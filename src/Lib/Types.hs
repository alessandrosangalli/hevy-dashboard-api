{-# LANGUAGE OverloadedStrings #-}

module Lib.Types
  ( FetchConfig(..)
  , defaultConfig
  , HttpRequest
  ) where

import Network.HTTP.Client (Request, Manager, Response)
import Data.ByteString.Lazy (ByteString)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)

-- | Configuration for fetching workouts.
data FetchConfig = FetchConfig
  { fcPageSize :: Int      -- ^ Number of workouts per page
  , fcRetries :: Int       -- ^ Number of retries for HTTP requests
  , fcDelay :: Int         -- ^ Delay between retries (in microseconds)
  , fcBaseUrl :: String    -- ^ Base URL for the API
  } deriving (Show)

-- | Default configuration for fetching workouts.
defaultConfig :: IO FetchConfig
defaultConfig = do
  maybeApiUrl <- lookupEnv "HEVY_API_URL"
  let baseUrl = fromMaybe "https://api.hevyapp.com" maybeApiUrl
  pure $ FetchConfig
    { fcPageSize = 10
    , fcRetries = 3
    , fcDelay = 1000000
    , fcBaseUrl = baseUrl
    }

-- | Type alias for an HTTP request function, injectable for testing.
type HttpRequest = Request -> Manager -> Int -> IO (Either String (Response ByteString))
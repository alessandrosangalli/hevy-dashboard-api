{-# LANGUAGE OverloadedStrings #-}

module Lib.HTTP
  ( fetchWorkoutsPage
  , defaultHttpRequest
  ) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Hevy (Workout, WorkoutsResponse, createdAt)  -- Adiciona createdAt aqui
import qualified Data.ByteString.Char8 as BS
import Control.Exception (try, SomeException)
import Control.Concurrent (threadDelay)
import Data.Time (UTCTime)
import Data.Text (unpack)
import Data.ByteString.Lazy (ByteString)
import Lib.Core (parseWorkoutsResponse, parseDate)
import Lib.Types (FetchConfig(..), HttpRequest, defaultConfig)

defaultHttpRequest :: HttpRequest
defaultHttpRequest req mgr retries = do
  result <- try $ httpLbs req mgr :: IO (Either SomeException (Response ByteString))
  case result of
    Left _ | retries > 0 -> do
      threadDelay (fcDelay defaultConfig)
      defaultHttpRequest req mgr (retries - 1)
    Left e -> pure $ Left $ "Request failed after retries: " ++ show e
    Right response -> pure $ Right response

fetchWorkoutsPage :: HttpRequest -> Manager -> FetchConfig -> String -> Int -> Maybe UTCTime -> IO (Either String (WorkoutsResponse, [Workout]))
fetchWorkoutsPage httpRequest manager config apiKey page maybeStartDate = do
  let url = "https://api.hevyapp.com/v1/workouts?page=" ++ show page ++ "&pageSize=" ++ show (fcPageSize config)
  request <- parseRequest url
  let requestWithAuth = request { requestHeaders = [("api-key", BS.pack apiKey)] }
  result <- httpRequest requestWithAuth manager (fcRetries config)
  case result of
    Left err -> pure $ Left err
    Right response -> do
      let parsed = parseWorkoutsResponse (responseBody response)
      case parsed of
        Left err -> pure $ Left err
        Right (resp, workouts) -> pure $ Right (resp, filterWorkouts workouts)
  where
    filterWorkouts ws = case maybeStartDate of
      Nothing -> ws
      Just startDate -> filter (\w -> maybe False (>= startDate) (parseDate (unpack (createdAt w)))) ws
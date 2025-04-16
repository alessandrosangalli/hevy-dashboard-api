{-# LANGUAGE OverloadedStrings #-}

module Lib.HTTP
  ( fetchWorkoutsPage
  , defaultHttpRequest
  ) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusIsSuccessful) -- Adicionado
import Hevy (Workout, WorkoutsResponse, title, startTime)
import qualified Data.ByteString.Char8 as BS
import Control.Exception (try)
import Control.Concurrent (threadDelay)
import Data.Time (UTCTime)
import Data.Text (Text, unpack)
import Data.ByteString.Lazy (ByteString)
import Data.Vector (Vector)
import Lib.Core (parseWorkoutsResponse, parseDate)
import Lib.Types (FetchConfig(..), HttpRequest)

-- | Default HTTP request function with retry logic for network errors.
defaultHttpRequest :: HttpRequest
defaultHttpRequest req mgr retries = do
  result <- try $ httpLbs req mgr :: IO (Either HttpException (Response ByteString))
  case result of
    Left (HttpExceptionRequest _ (StatusCodeException _ _)) -> pure $ Left "Non-recoverable HTTP error"
    Left _ | retries > 0 -> threadDelay 1000000 >> defaultHttpRequest req mgr (retries - 1)
    Left e -> pure $ Left $ "Request failed after retries: " ++ show e
    Right response -> pure $ Right response

-- | Fetches a single page of workouts from the API.
fetchWorkoutsPage :: HttpRequest -> Manager -> FetchConfig -> String -> Int -> Maybe UTCTime -> IO (Either String (WorkoutsResponse, Vector Workout))
fetchWorkoutsPage httpRequest manager config apiKey page maybeStartDate = do
  let url = fcBaseUrl config <> "/v1/workouts?page=" <> show page <> "&pageSize=" <> show (fcPageSize config)
  request <- parseRequest url
  let requestWithAuth = request { requestHeaders = [("api-key", BS.pack apiKey)] }
  result <- httpRequest requestWithAuth manager (fcRetries config)
  case result of
    Left err -> pure $ Left err
    Right response -> do
      let status = responseStatus response
      if statusIsSuccessful status
        then case parseWorkoutsResponse (responseBody response) of
          Left err -> pure $ Left $ "Failed to parse page " <> show page <> ": " <> err
          Right (resp, workouts) -> pure $ Right (resp, workouts)
        else pure $ Left $ "HTTP error on page " <> show page <> ": " <> show status
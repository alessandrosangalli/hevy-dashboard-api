{-# LANGUAGE OverloadedStrings #-}

module Lib (fetchAllWorkouts, fetchAllWorkoutsWithLog) where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Hevy (Workout, WorkoutsResponse(..))
import qualified Data.ByteString.Char8 as BS
import Control.Concurrent.Async (mapConcurrently)
import Data.List (foldl')
import System.Environment (lookupEnv)

-- ** Functional Core **

parseWorkoutsResponse :: ByteString -> Either String (WorkoutsResponse, [Workout])
parseWorkoutsResponse body = case eitherDecode body of
  Left err -> Left err
  Right resp -> Right (resp, workouts resp)

calculateTotalPages :: WorkoutsResponse -> Int
calculateTotalPages = max 1 . page_count

combinePageResults :: Either String [Workout] -> Either String [Workout] -> Either String [Workout]
combinePageResults (Left err) _ = Left err
combinePageResults _ (Left err) = Left err
combinePageResults (Right acc) (Right ws) = Right (acc ++ ws)

combineAllPages :: [Workout] -> [Either String [Workout]] -> Either String [Workout]
combineAllPages initial others = foldl' combinePageResults (Right initial) others

-- ** Imperative Shell **

data FetchConfig = FetchConfig
  { fcPageSize :: Int
  , fcMaxConcurrent :: Int
  }

defaultConfig :: FetchConfig
defaultConfig = FetchConfig 10 4

fetchWorkoutsPage :: Manager -> FetchConfig -> String -> Int -> IO (Either String (WorkoutsResponse, [Workout]))
fetchWorkoutsPage manager config apiKey page = do
  let url = "https://api.hevyapp.com/v1/workouts?page=" ++ show page ++ "&pageSize=" ++ show (fcPageSize config)
  request <- parseRequest url
  let requestWithAuth = request { requestHeaders = [("api-key", BS.pack apiKey)] }
  response <- httpLbs requestWithAuth manager
  pure $ parseWorkoutsResponse (responseBody response)

fetchAllWorkoutsWithConfigAndLog :: FetchConfig -> (String -> IO ()) -> IO (Either String [Workout])
fetchAllWorkoutsWithConfigAndLog config logger = do
  maybeApiKey <- lookupEnv "HEVY_API_KEY"
  case maybeApiKey of
    Nothing -> do
      logger "Erro: The environment variable HEVY_API_KEY is not defined."
      pure $ Left "The environment variable HEVY_API_KEY is not defined."
    Just apiKey -> do
      manager <- newManager tlsManagerSettings
      logger $ "Starting search with pageSize=" ++ show (fcPageSize config)
      firstPageResult <- fetchWorkoutsPage manager config apiKey 1
      case firstPageResult of
        Left err -> do
          logger $ "Err in the first page: " ++ err
          pure (Left err)
        Right (resp, ws1) -> do
          let totalPages = calculateTotalPages resp
          logger $ "Total pages: " ++ show totalPages
          if totalPages == 1
            then pure (Right ws1)
            else do
              let remainingPages = [2..totalPages]
              logger $ "Searching " ++ show (length remainingPages) ++ " remaining pages in parallel"
              results <- mapConcurrently (fetchAndLogPage manager apiKey) remainingPages
              logger "Combine results"
              pure $ combineAllPages ws1 results
  where
    fetchAndLogPage manager apiKey page = do
      logger $ "Searching page " ++ show page
      res <- fetchWorkoutsPage manager config apiKey page
      case res of
        Left err -> logger $ "Error in page " ++ show page ++ ": " ++ err
        Right (_, ws) -> logger $ "Recieved " ++ show (length ws) ++ " workouts in page " ++ show page
      pure (fmap snd res)

-- ** Public API **

fetchAllWorkouts :: IO (Either String [Workout])
fetchAllWorkouts = fetchAllWorkoutsWithConfig defaultConfig

fetchAllWorkoutsWithLog :: (String -> IO ()) -> IO (Either String [Workout])
fetchAllWorkoutsWithLog = fetchAllWorkoutsWithConfigAndLog defaultConfig

fetchAllWorkoutsWithConfig :: FetchConfig -> IO (Either String [Workout])
fetchAllWorkoutsWithConfig config = fetchAllWorkoutsWithConfigAndLog config (const $ pure ())
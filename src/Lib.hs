{-# LANGUAGE OverloadedStrings #-}

module Lib (fetchAllWorkouts, fetchAllWorkoutsWithLog, fetchAllWorkoutsGrouped, fetchAllWorkoutsGroupedWithLog) where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Hevy (Workout, WorkoutsResponse(..), createdAt)
import qualified Data.ByteString.Char8 as BS
import Control.Exception (try, SomeException)
import Control.Concurrent (threadDelay)
import Data.List (foldl', groupBy, sortOn)
import System.Environment (lookupEnv)
import Data.Time (UTCTime, utctDay, parseTimeM, defaultTimeLocale, diffDays)
import Data.Text (unpack)
import Control.Concurrent.Async (mapConcurrently)

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

parseDate :: String -> Maybe UTCTime
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"

groupWorkoutsByWeeks :: [Workout] -> [[Workout]]
groupWorkoutsByWeeks ws =
  let
    sortedWorkouts = sortOn (parseDate . Data.Text.unpack . createdAt) ws
    groupByInterval w1 w2 = case (parseDate (Data.Text.unpack $ createdAt w1), parseDate (Data.Text.unpack $ createdAt w2)) of
      (Just date1, Just date2) -> diffDays (utctDay date2) (utctDay date1) < 28
      _ -> False
  in groupBy groupByInterval sortedWorkouts

-- ** Imperative Shell **

data FetchConfig = FetchConfig
  { fcPageSize :: Int
  , fcRetries :: Int
  , fcDelay :: Int -- in microseconds
  }

defaultConfig :: FetchConfig
defaultConfig = FetchConfig 10 3 1000000

fetchWorkoutsPage :: Manager -> FetchConfig -> String -> Int -> Maybe UTCTime -> IO (Either String (WorkoutsResponse, [Workout]))
fetchWorkoutsPage manager config apiKey page maybeStartDate = do
  let url = "https://api.hevyapp.com/v1/workouts?page=" ++ show page ++ "&pageSize=" ++ show (fcPageSize config)
  request <- parseRequest url
  let requestWithAuth = request { requestHeaders = [("api-key", BS.pack apiKey)] }
  result <- attemptRequest requestWithAuth manager (fcRetries config)
  case result of
    Left err -> pure $ Left err
    Right (resp, workouts) -> pure $ Right (resp, filterWorkouts workouts)
  where
    attemptRequest req mgr retries = do
      result <- try $ httpLbs req mgr :: IO (Either SomeException (Response ByteString))
      case result of
        Left _ | retries > 0 -> do
          threadDelay (fcDelay config)
          attemptRequest req mgr (retries - 1)
        Left e -> pure $ Left $ "Request failed after retries: " ++ show e
        Right response -> pure $ parseWorkoutsResponse (responseBody response)
    filterWorkouts ws = case maybeStartDate of
      Nothing -> ws
      Just startDate -> filter (\w -> maybe False (>= startDate) (parseDate (Data.Text.unpack (createdAt w)))) ws

fetchAllWorkoutsWithConfigAndLog :: FetchConfig -> Maybe UTCTime -> (String -> IO ()) -> IO (Either String [Workout])
fetchAllWorkoutsWithConfigAndLog config maybeStartDate logger = do
  maybeApiKey <- lookupEnv "HEVY_API_KEY"
  case maybeApiKey of
    Nothing -> do
      logger "Erro: The environment variable HEVY_API_KEY is not defined."
      pure $ Left "The environment variable HEVY_API_KEY is not defined."
    Just apiKey -> do
      manager <- newManager tlsManagerSettings
      logger $ "Starting search with pageSize=" ++ show (fcPageSize config)
      firstPageResult <- fetchWorkoutsPage manager config apiKey 1 maybeStartDate
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
              results <- mapConcurrently (fetchAndLogPage manager apiKey maybeStartDate) remainingPages
              logger "Combine results"
              pure $ combineAllPages ws1 results
  where
    fetchAndLogPage manager apiKey startDate page = do
      logger $ "Searching page " ++ show page
      res <- fetchWorkoutsPage manager config apiKey page startDate
      case res of
        Left err -> logger $ "Error in page " ++ show page ++ ": " ++ err
        Right (_, ws) -> logger $ "Received " ++ show (length ws) ++ " workouts in page " ++ show page
      pure (fmap snd res)

-- ** Public API **

fetchAllWorkouts :: IO (Either String [Workout])
fetchAllWorkouts = fetchAllWorkoutsWithConfig defaultConfig

fetchAllWorkoutsWithLog :: (String -> IO ()) -> IO (Either String [Workout])
fetchAllWorkoutsWithLog logger = fetchAllWorkoutsWithConfigAndLog defaultConfig Nothing logger

fetchAllWorkoutsWithConfig :: FetchConfig -> IO (Either String [Workout])
fetchAllWorkoutsWithConfig config = fetchAllWorkoutsWithConfigAndLog config Nothing (const $ pure ())

fetchAllWorkoutsGrouped :: IO (Either String [[Workout]])
fetchAllWorkoutsGrouped = do
  result <- fetchAllWorkouts
  pure $ fmap groupWorkoutsByWeeks result

fetchAllWorkoutsGroupedWithLog :: Maybe UTCTime -> (String -> IO ()) -> IO (Either String [[Workout]])
fetchAllWorkoutsGroupedWithLog maybeStartDate logger = do
  result <- fetchAllWorkoutsWithConfigAndLog defaultConfig maybeStartDate logger
  pure $ fmap (filterWorkoutsByStartDate maybeStartDate . groupWorkoutsByWeeks) result

filterWorkoutsByStartDate :: Maybe UTCTime -> [[Workout]] -> [[Workout]]
filterWorkoutsByStartDate Nothing groups = groups
filterWorkoutsByStartDate (Just startDate) groups =
  filter (not . null) $ map (filter (\w -> maybe False (>= startDate) (parseDate (Data.Text.unpack (createdAt w))))) groups
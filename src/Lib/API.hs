{-# LANGUAGE OverloadedStrings #-}

module Lib.API
  ( fetchAllWorkouts
  , fetchAllWorkoutsWithLog
  , fetchAllWorkoutsGrouped
  , fetchAllWorkoutsGroupedWithLog
  , fetchAllWorkoutsWithConfig
  , fetchAllWorkoutsWithConfigAndLog
  ) where

import Hevy (Workout, WorkoutsResponse, title, startTime)
import System.Environment (lookupEnv)
import Data.Time (UTCTime)
import Control.Concurrent.Async (mapConcurrently)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Lib.Core (combineAllPages, calculateTotalPages, groupWorkoutsByWeeks, formatWorkoutsOutput, parseDate)
import Lib.HTTP (fetchWorkoutsPage, defaultHttpRequest)
import Lib.Types (FetchConfig(..), defaultConfig, HttpRequest)
import Data.ByteString.Lazy (ByteString)
import Data.Text (unpack)

fetchAllWorkoutsWithConfigAndLog :: FetchConfig -> Maybe UTCTime -> (String -> IO ()) -> HttpRequest -> IO (Either String [Workout])
fetchAllWorkoutsWithConfigAndLog config maybeStartDate logger httpRequest = do
  maybeApiKey <- lookupEnv "HEVY_API_KEY"
  case maybeApiKey of
    Nothing -> do
      logger "Erro: The environment variable HEVY_API_KEY is not defined."
      pure $ Left "The environment variable HEVY_API_KEY is not defined."
    Just apiKey -> do
      manager <- newManager tlsManagerSettings
      logger $ "Starting search with pageSize=" ++ show (fcPageSize config)
      firstPageResult <- fetchWorkoutsPage httpRequest manager config apiKey 1 maybeStartDate
      case firstPageResult of
        Left err -> do
          logger $ "Err in the first page: " ++ err
          pure $ Left err
        Right (resp, ws1) -> do
          let totalPages = calculateTotalPages resp
          logger $ "Total pages: " ++ show totalPages
          if totalPages == 1
            then pure $ Right ws1
            else do
              let remainingPages = [2..totalPages]
              logger $ "Searching " ++ show (length remainingPages) ++ " remaining pages in parallel"
              results <- mapConcurrently (fetchAndLogPage manager apiKey) remainingPages
              logger "Combine results"
              pure $ combineAllPages ws1 results
  where
    fetchAndLogPage manager apiKey page = do
      logger $ "Searching page " ++ show page
      res <- fetchWorkoutsPage httpRequest manager config apiKey page maybeStartDate
      case res of
        Left err -> do
          logger $ "Error in page " ++ show page ++ ": " ++ err
          pure $ Left err
        Right (resp, ws) -> do
          logger $ "Received " ++ show (length ws) ++ " workouts in page " ++ show page
          pure $ Right ws

fetchAllWorkouts :: IO (Either String [Workout])
fetchAllWorkouts = fetchAllWorkoutsWithConfigAndLog defaultConfig Nothing (const $ pure ()) defaultHttpRequest

fetchAllWorkoutsWithLog :: (String -> IO ()) -> IO (Either String [Workout])
fetchAllWorkoutsWithLog logger = fetchAllWorkoutsWithConfigAndLog defaultConfig Nothing logger defaultHttpRequest

fetchAllWorkoutsWithConfig :: FetchConfig -> IO (Either String [Workout])
fetchAllWorkoutsWithConfig config = fetchAllWorkoutsWithConfigAndLog config Nothing (const $ pure ()) defaultHttpRequest

fetchAllWorkoutsGrouped :: IO (Either String ByteString)
fetchAllWorkoutsGrouped = fetchAllWorkoutsGroupedWithLog "" (const $ pure ())

fetchAllWorkoutsGroupedWithLog :: String -> (String -> IO ()) -> IO (Either String ByteString)
fetchAllWorkoutsGroupedWithLog startDateStr logger = do
  logger $ "Received startDateStr: " ++ show startDateStr
  let maybeStartDate = if null startDateStr then Nothing else parseDate startDateStr
  logger $ "Parsed startDate: " ++ show maybeStartDate
  result <- fetchAllWorkoutsWithConfigAndLog defaultConfig maybeStartDate logger defaultHttpRequest
  case result of
    Left err -> pure $ Left err
    Right workouts -> do
      logger $ "Total workouts before filtering: " ++ show (length workouts)
      case maybeStartDate of
        Just startDate -> mapM_ (logger . logWorkout startDate) workouts
        Nothing -> logger "No valid startDate, including all workouts"
      let filteredWorkouts = case maybeStartDate of
            Nothing -> workouts
            Just startDate -> filter (\w -> maybe False (>= startDate) (startTime w >>= parseDate . unpack)) workouts
      logger $ "Total workouts after filtering: " ++ show (length filteredWorkouts)
      case maybeStartDate of
        Just startDate -> mapM_ (logger . logFilteredWorkout startDate) filteredWorkouts
        Nothing -> pure ()
      let groups = groupWorkoutsByWeeks maybeStartDate filteredWorkouts
      pure $ Right $ formatWorkoutsOutput maybeStartDate groups
  where
    logWorkout startDate w = 
      let parsedTime = startTime w >>= parseDate . unpack
          keep = maybe False (>= startDate) parsedTime
      in "Before filter - Workout: title=" ++ unpack (title w) ++ 
         ", start_time=" ++ show (startTime w) ++ 
         ", parsed=" ++ show parsedTime ++ 
         ", keep=" ++ show keep
    logFilteredWorkout startDate w = 
      let parsedTime = startTime w >>= parseDate . unpack
      in "After filter - Workout: title=" ++ unpack (title w) ++ 
         ", start_time=" ++ show (startTime w) ++ 
         ", parsed=" ++ show parsedTime
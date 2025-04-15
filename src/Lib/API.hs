{-# LANGUAGE OverloadedStrings #-}

module Lib.API
  ( fetchAllWorkouts
  , fetchAllWorkoutsWithLog
  , fetchAllWorkoutsGrouped
  , fetchAllWorkoutsGroupedWithLog
  , fetchAllWorkoutsWithConfig
  , fetchAllWorkoutsWithConfigAndLog
  ) where

import Hevy (Workout, WorkoutsResponse)
import System.Environment (lookupEnv)
import Data.Time (UTCTime)
import Control.Concurrent.Async (mapConcurrently)
import Network.HTTP.Client (newManager)  -- Adiciona esta importação
import Network.HTTP.Client.TLS (tlsManagerSettings)  -- Adiciona esta importação
import Lib.Core (combineAllPages, calculateTotalPages, groupWorkoutsByWeeks, filterWorkoutsByStartDate)
import Lib.HTTP (fetchWorkoutsPage, defaultHttpRequest)
import Lib.Types (FetchConfig(..), defaultConfig, HttpRequest)

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

fetchAllWorkoutsGrouped :: IO (Either String [[Workout]])
fetchAllWorkoutsGrouped = do
  result <- fetchAllWorkouts
  pure $ fmap groupWorkoutsByWeeks result

fetchAllWorkoutsGroupedWithLog :: Maybe UTCTime -> (String -> IO ()) -> IO (Either String [[Workout]])
fetchAllWorkoutsGroupedWithLog maybeStartDate logger = do
  result <- fetchAllWorkoutsWithConfigAndLog defaultConfig maybeStartDate logger defaultHttpRequest
  pure $ fmap (filterWorkoutsByStartDate maybeStartDate . groupWorkoutsByWeeks) result
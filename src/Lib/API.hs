{-# LANGUAGE OverloadedStrings #-}

module Lib.API
  ( fetchAllWorkouts
  , fetchAllWorkoutsWithLog
  , fetchAllWorkoutsGrouped
  , fetchAllWorkoutsGroupedWithLog
  , fetchAllWorkoutsWithConfig
  , fetchAllWorkoutsWithConfigAndLog
  ) where

import Hevy (Workout, WorkoutsResponse, title, startTime, createdAt)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime)
import Control.Concurrent.Async (mapConcurrently)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Lib.Core (combineAllPages, calculateTotalPages, groupWorkoutsByWeeks, formatWorkoutsOutput, parseDate)
import Lib.HTTP (fetchWorkoutsPage, defaultHttpRequest)
import Lib.Types (FetchConfig(..), defaultConfig, HttpRequest)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text, unpack, pack)
import qualified Data.Vector as V
import Data.Vector.Algorithms.Intro (sortBy)
import Data.Function (on)

-- | Fetches all workouts with custom configuration and logging.
fetchAllWorkoutsWithConfigAndLog :: FetchConfig -> Maybe UTCTime -> (Text -> IO ()) -> HttpRequest -> IO (Either String (V.Vector Workout))
fetchAllWorkoutsWithConfigAndLog config maybeStartDate logger httpRequest = do
  maybeApiKey <- lookupEnv "HEVY_API_KEY"
  case maybeApiKey of
    Nothing -> do
      logger "Error: HEVY_API_KEY environment variable not set."
      pure $ Left "HEVY_API_KEY not set."
    Just apiKey -> do
      manager <- newManager tlsManagerSettings
      logger $ "Starting fetch with pageSize=" <> pack (show $ fcPageSize config)
      firstPageResult <- fetchWorkoutsPage httpRequest manager config apiKey 1 maybeStartDate
      case firstPageResult of
        Left err -> do
          logger $ "Error fetching first page: " <> pack err
          pure $ Left err
        Right (resp, ws1) -> do
          let totalPages = calculateTotalPages resp
          logger $ "Total pages: " <> pack (show totalPages)
          if totalPages == 1
            then pure $ Right ws1
            else do
              let remainingPages = [2..totalPages]
              logger $ "Fetching " <> pack (show $ length remainingPages) <> " remaining pages in parallel"
              results <- mapConcurrently (fetchAndLogPage manager apiKey) remainingPages
              logger "Combining results"
              let combined = combineAllPages ws1 results
              pure $ fmap (V.modify (\v -> sortBy (compare `on` (parseDate . unpack . createdAt)) v)) combined
  where
    fetchAndLogPage manager apiKey page = do
      logger $ "Fetching page " <> pack (show page)
      res <- fetchWorkoutsPage httpRequest manager config apiKey page maybeStartDate
      case res of
        Left err -> do
          logger $ "Error on page " <> pack (show page) <> ": " <> pack err
          pure $ Left err
        Right (resp, ws) -> do
          logger $ "Received " <> pack (show $ V.length ws) <> " workouts on page " <> pack (show page)
          pure $ Right ws

-- | Fetches all workouts with default configuration.
fetchAllWorkouts :: IO (Either String (V.Vector Workout))
fetchAllWorkouts = do
  config <- defaultConfig
  fetchAllWorkoutsWithConfigAndLog config Nothing (const $ pure ()) defaultHttpRequest

-- | Fetches all workouts with logging.
fetchAllWorkoutsWithLog :: (Text -> IO ()) -> IO (Either String (V.Vector Workout))
fetchAllWorkoutsWithLog logger = do
  config <- defaultConfig
  fetchAllWorkoutsWithConfigAndLog config Nothing logger defaultHttpRequest

-- | Fetches all workouts with custom configuration.
fetchAllWorkoutsWithConfig :: FetchConfig -> IO (Either String (V.Vector Workout))
fetchAllWorkoutsWithConfig config = fetchAllWorkoutsWithConfigAndLog config Nothing (const $ pure ()) defaultHttpRequest

-- | Fetches workouts grouped by week.
fetchAllWorkoutsGrouped :: IO (Either String ByteString)
fetchAllWorkoutsGrouped = fetchAllWorkoutsGroupedWithLog "" (const $ pure ())

-- | Fetches workouts grouped by week with logging.
fetchAllWorkoutsGroupedWithLog :: String -> (Text -> IO ()) -> IO (Either String ByteString)
fetchAllWorkoutsGroupedWithLog startDateStr logger = do
  logger $ "Received startDateStr: " <> pack (show startDateStr)
  let maybeStartDate = if null startDateStr then Nothing else parseDate startDateStr
  logger $ "Parsed startDate: " <> pack (show maybeStartDate)
  config <- defaultConfig
  result <- fetchAllWorkoutsWithConfigAndLog config maybeStartDate logger defaultHttpRequest
  case result of
    Left err -> pure $ Left err
    Right workouts -> do
      logger $ "Total workouts before filtering: " <> pack (show $ V.length workouts)
      let filteredWorkouts = case maybeStartDate of
            Nothing -> workouts
            Just startDate -> V.filter (\w -> fromMaybe False $ do
              t <- startTime w
              d <- parseDate (unpack t)
              pure (d >= startDate)) workouts
      logger $ "Total workouts after filtering: " <> pack (show $ V.length filteredWorkouts)
      let groups = groupWorkoutsByWeeks maybeStartDate filteredWorkouts
      pure $ Right $ formatWorkoutsOutput maybeStartDate groups
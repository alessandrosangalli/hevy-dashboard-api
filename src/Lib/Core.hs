{-# LANGUAGE OverloadedStrings #-}

module Lib.Core
  ( parseWorkoutsResponse
  , calculateTotalPages
  , combinePageResults
  , combineAllPages
  , parseDate
  , groupWorkoutsByWeeks
  , filterWorkoutsByStartDate
  ) where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (ByteString)
import Hevy (Workout, WorkoutsResponse(..), createdAt)
import Data.List (foldl', groupBy, sortOn)
import Data.Time (UTCTime, utctDay, parseTimeM, defaultTimeLocale, diffDays)
import Data.Text (unpack)

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
  let sortedWorkouts = sortOn (parseDate . unpack . createdAt) ws
      groupByInterval w1 w2 = case (parseDate (unpack $ createdAt w1), parseDate (unpack $ createdAt w2)) of
        (Just date1, Just date2) -> diffDays (utctDay date2) (utctDay date1) < 28
        _ -> False
  in groupBy groupByInterval sortedWorkouts

filterWorkoutsByStartDate :: Maybe UTCTime -> [[Workout]] -> [[Workout]]
filterWorkoutsByStartDate Nothing groups = groups
filterWorkoutsByStartDate (Just startDate) groups =
  filter (not . null) $ map (filter (\w -> maybe False (>= startDate) (parseDate (unpack (createdAt w))))) groups
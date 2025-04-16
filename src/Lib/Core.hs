{-# LANGUAGE OverloadedStrings #-}

module Lib.Core
  ( parseWorkoutsResponse
  , calculateTotalPages
  , combinePageResults
  , combineAllPages
  , parseDate
  , groupWorkoutsByWeeks
  , formatWorkoutsOutput
  ) where

import Data.Aeson (eitherDecode, encode, object, (.=), ToJSON(..))
import Data.ByteString.Lazy (ByteString)
import Hevy (Workout, WorkoutsResponse(..), createdAt, title, exercises, startTime)
import Data.List (foldl', groupBy, sortOn)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime, utctDay, parseTimeM, defaultTimeLocale, diffDays, addDays)
import Data.Text (unpack)
import Control.Applicative ((<|>))
import Control.Monad ((>=>))

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
parseDate s = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Z" s
  <|> parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" s
  <|> parseTimeM True defaultTimeLocale "%Y-%m-%d" s

groupWorkoutsByWeeks :: Maybe UTCTime -> [Workout] -> [[Workout]]
groupWorkoutsByWeeks _ ws = take 4 $ groupByWeeks ws ++ repeat []

groupByWeeks :: [Workout] -> [[Workout]]
groupByWeeks ws =
  let sortedWorkouts = sortOn (startTime >=> parseDate . unpack) ws
      groupByWeek w1 w2 = case (startTime w1 >>= parseDate . unpack, startTime w2 >>= parseDate . unpack) of
        (Just date1, Just date2) -> let weekDiff = diffDays (utctDay date2) (utctDay date1) `div` 7 in weekDiff == 0
        _ -> False
  in groupBy groupByWeek sortedWorkouts

formatWorkoutsOutput :: Maybe UTCTime -> [[Workout]] -> ByteString
formatWorkoutsOutput maybeStartDate groups =
  let weekGroups = take 4 $ zip [1..] groups ++ [(i, []) | i <- [length groups + 1 .. 4]]
      toWeekObject (weekNum, ws) = object
        [ "week" .= weekNum
        , "workouts" .= map (\w -> [object
            [ "name" .= title w
            , "start_time" .= fromMaybe "" (startTime w)
            , "exercises" .= exercises w
            ]]) ws
        ]
  in encode $ map toWeekObject weekGroups
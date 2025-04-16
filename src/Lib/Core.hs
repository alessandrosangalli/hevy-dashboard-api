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
import Hevy (Workout, WorkoutsResponse(..), createdAt, title, exercises, startTime, exerciseName, exerciseSets)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Algorithms.Intro (sortBy)
import Data.List (groupBy)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime, utctDay, parseTimeM, defaultTimeLocale, addDays)
import Data.Time.Calendar (toGregorian, fromGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Text (unpack)
import Data.Function (on)
import Control.Applicative ((<|>))
import Control.Monad ((>=>))
import Prelude hiding ((++))

-- | Parses a JSON response into a WorkoutsResponse and Vector of Workouts.
parseWorkoutsResponse :: ByteString -> Either String (WorkoutsResponse, Vector Workout)
parseWorkoutsResponse body = case eitherDecode body of
  Left err -> Left $ "Failed to parse JSON response: " <> err
  Right resp -> Right (resp, V.fromList $ workouts resp)

-- | Calculates the total number of pages, ensuring at least 1.
calculateTotalPages :: WorkoutsResponse -> Int
calculateTotalPages = max 1 . page_count

-- | Combines two page results, preserving errors.
combinePageResults :: Either String (Vector Workout) -> Either String (Vector Workout) -> Either String (Vector Workout)
combinePageResults (Left err) _ = Left err
combinePageResults _ (Left err) = Left err
combinePageResults (Right acc) (Right ws) = Right (acc V.++ ws)

-- | Combines all page results into a single Vector of workouts.
combineAllPages :: Vector Workout -> [Either String (Vector Workout)] -> Either String (Vector Workout)
combineAllPages initial others =
  let combined = foldr combinePageResults (Right initial) others
  in fmap (V.modify (\v -> sortBy (compare `on` (parseDate . unpack . createdAt)) v)) combined

-- | Parses a date string into UTCTime, supporting multiple formats.
parseDate :: String -> Maybe UTCTime
parseDate s = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Z" s
  <|> parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" s
  <|> parseTimeM True defaultTimeLocale "%Y-%m-%d" s

-- | Groups workouts by weeks, returning up to 4 weeks.
groupWorkoutsByWeeks :: Maybe UTCTime -> Vector Workout -> [[Workout]]
groupWorkoutsByWeeks _ ws = take 4 $ groupByWeeks ws <> repeat []

-- | Groups workouts by calendar weeks using ISO week dates.
groupByWeeks :: Vector Workout -> [[Workout]]
groupByWeeks ws =
  let sorted = V.modify (\v -> sortBy (compare `on` (startTime >=> parseDate . unpack)) v) ws
      weekNum w = maybe (9999, 99) (\d -> 
        let (y, m, day) = toGregorian (utctDay d)
            -- Adjust to start of week (Monday)
            startOfWeek = fromGregorian y m (day - (mod (dayOfWeek d) 7))
            (weekYear, week, _) = toWeekDate startOfWeek
        in (weekYear, week)) $ startTime w >>= parseDate . unpack
      dayOfWeek d = let (_, _, dow) = toWeekDate (utctDay d) in if dow == 0 then 7 else dow
      groups = groupBy (\w1 w2 -> weekNum w1 == weekNum w2) (V.toList sorted)
  in groups

-- | Formats workout groups into a JSON-encoded ByteString.
formatWorkoutsOutput :: Maybe UTCTime -> [[Workout]] -> ByteString
formatWorkoutsOutput maybeStartDate groups =
  let nonEmptyGroups = filter (not . null) groups
      weekGroups = zip ([1..] :: [Int]) nonEmptyGroups
      toWeekObject (weekNum, ws) = object
        [ "week" .= weekNum
        , "workouts" .= map (\w -> object
            [ "name" .= title w
            , "start_time" .= fromMaybe "" (startTime w)
            , "exercises" .= map (\e -> object
                [ "name" .= exerciseName e
                , "sets" .= exerciseSets e
                ]) (exercises w)
            ]) ws
        ]
  in encode $ map toWeekObject weekGroups
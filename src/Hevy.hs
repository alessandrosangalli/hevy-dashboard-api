{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Hevy (Workout(..), WorkoutsResponse(..), Exercise(..), Set(..)) where

import Data.Aeson (FromJSON, ToJSON(toJSON), parseJSON, withObject, (.:), (.:?), object, (.=))
import Data.Maybe (fromMaybe) -- Adicionado
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A single set in an exercise, including repetitions, weight, type, and RPE.
data Set = Set
  { setReps :: Maybe Int
  , setWeightKg :: Maybe Double
  , setType :: Text
  , setRpe :: Maybe Double
  } deriving (Show, Eq, Generic)

instance FromJSON Set where
  parseJSON = withObject "Set" $ \v -> Set
    <$> v .:? "reps"
    <*> v .:? "weight_kg"
    <*> v .: "type"
    <*> v .:? "rpe"

instance ToJSON Set

-- | An exercise within a workout, containing a name and a list of sets.
data Exercise = Exercise
  { exerciseName :: Text
  , exerciseSets :: [Set]
  } deriving (Show, Eq, Generic)

instance FromJSON Exercise where
  parseJSON = withObject "Exercise" $ \v -> Exercise
    <$> v .: "title"
    <*> (fromMaybe [] <$> v .:? "sets")

instance ToJSON Exercise where
  toJSON (Exercise name sets) = object
    [ "name" .= name
    , "sets" .= sets
    ]

-- | A workout, including its ID, title, creation time, start time, and exercises.
data Workout = Workout
  { workoutId :: Text
  , title :: Text
  , createdAt :: Text
  , startTime :: Maybe Text
  , exercises :: [Exercise]
  } deriving (Show, Eq, Generic)

instance FromJSON Workout where
  parseJSON = withObject "Workout" $ \v -> Workout
    <$> v .: "id"
    <*> v .: "title"
    <*> v .: "created_at"
    <*> v .:? "start_time"
    <*> v .: "exercises"

instance ToJSON Workout where
  toJSON (Workout workoutId title createdAt startTime exercises) = object
    [ "id" .= workoutId
    , "title" .= title
    , "created_at" .= createdAt
    , "start_time" .= startTime
    , "exercises" .= exercises
    ]

-- | Response from the workouts API, containing a list of workouts and page count.
data WorkoutsResponse = WorkoutsResponse
  { workouts :: [Workout]
  , page_count :: Int
  } deriving (Show, Eq, Generic)

instance FromJSON WorkoutsResponse where
  parseJSON = withObject "WorkoutsResponse" $ \v -> WorkoutsResponse
    <$> v .: "workouts"
    <*> v .: "page_count"

instance ToJSON WorkoutsResponse
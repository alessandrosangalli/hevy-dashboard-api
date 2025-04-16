{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Hevy (Workout(..), WorkoutsResponse(..), Exercise(..), Set(..)) where

import Data.Aeson (FromJSON, ToJSON(toJSON), parseJSON, withObject, (.:), (.:?), object, (.=))
import Data.Aeson.Types (Parser)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)

data Set = Set
  { setReps :: Maybe Int
  , setWeightKg :: Maybe Double
  , setType :: Text
  , setRpe :: Maybe Double  -- Added rpe
  } deriving (Show, Eq, Generic)

instance FromJSON Set where
  parseJSON = withObject "Set" $ \v -> Set
    <$> v .:? "reps"
    <*> v .:? "weight_kg"
    <*> v .: "type"
    <*> v .:? "rpe"

instance ToJSON Set

data Exercise = Exercise
  { exerciseName :: Text
  , sets :: Int
  , reps :: Int
  , weightKg :: Maybe Double  -- Added weight_kg
  , rpe :: Maybe Double       -- Added rpe
  } deriving (Show, Eq, Generic)

instance FromJSON Exercise where
  parseJSON = withObject "Exercise" $ \v -> Exercise
    <$> v .: "title"
    <*> (maybe 0 length <$> (v .:? "sets" :: Parser (Maybe [Set])))
    <*> (v .:? "sets" >>= \mSets -> case mSets of
          Nothing -> pure 0
          Just [] -> pure 0
          Just (s:_) -> pure (fromMaybe 0 (setReps s)))
    <*> (v .:? "sets" >>= \mSets -> case mSets of
          Nothing -> pure Nothing
          Just [] -> pure Nothing
          Just (s:_) -> pure (setWeightKg s))
    <*> (v .:? "sets" >>= \mSets -> case mSets of
          Nothing -> pure Nothing
          Just [] -> pure Nothing
          Just (s:_) -> pure (setRpe s))

instance ToJSON Exercise where
  toJSON (Exercise name sets reps weightKg rpe) = object
    [ "name" .= name
    , "sets" .= sets
    , "reps" .= reps
    , "weight_kg" .= weightKg
    , "rpe" .= rpe
    ]

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

data WorkoutsResponse = WorkoutsResponse
  { workouts :: [Workout]
  , page_count :: Int 
  } deriving (Show, Eq, Generic)

instance FromJSON WorkoutsResponse where
  parseJSON = withObject "WorkoutsResponse" $ \v -> WorkoutsResponse
    <$> v .: "workouts"
    <*> v .: "page_count"

instance ToJSON WorkoutsResponse
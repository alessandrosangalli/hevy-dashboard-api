{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Hevy (Workout(..), WorkoutsResponse(..)) where

import Data.Aeson (FromJSON, ToJSON, parseJSON, withObject, (.:))
import Data.Text (Text)
import GHC.Generics (Generic)

data Workout = Workout
  { workoutId :: Text
  , title :: Text
  , createdAt :: Text
  } deriving (Show, Generic)

instance FromJSON Workout where
  parseJSON = withObject "Workout" $ \v -> Workout
    <$> v .: "id"
    <*> v .: "title"
    <*> v .: "created_at"

instance ToJSON Workout

data WorkoutsResponse = WorkoutsResponse
  { workouts :: [Workout]
  , page_count :: Int 
  } deriving (Show, Generic)

instance FromJSON WorkoutsResponse where
  parseJSON = withObject "WorkoutsResponse" $ \v -> WorkoutsResponse
    <$> v .: "workouts"
    <*> v .: "page_count"

instance ToJSON WorkoutsResponse
{-# LANGUAGE OverloadedStrings #-}

module HevySpec (spec) where

import Test.Hspec
import Data.Aeson (eitherDecode)
import Data.Either (isLeft)
import Data.Text (pack)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Hevy (Workout(..), WorkoutsResponse(..))

spec :: Spec
spec = do
  describe "Workout JSON parsing" $ do
    it "parses valid Workout JSON" $ do
      let json = LBS.pack "{\"id\":\"1\",\"title\":\"Morning Run\",\"created_at\":\"2023-01-01T00:00:00Z\",\"start_time\":null,\"exercises\":[]}"
      eitherDecode json `shouldBe` Right (Workout (pack "1") (pack "Morning Run") (pack "2023-01-01T00:00:00Z") Nothing [])
    it "fails on missing required fields" $ do
      let json = LBS.pack "{\"id\":\"1\",\"title\":\"Morning Run\"}"
      (eitherDecode json :: Either String Workout) `shouldSatisfy` isLeft
    it "fails on invalid JSON structure" $ do
      let json = LBS.pack "{\"id\":1,\"title\":\"Morning Run\",\"created_at\":\"2023-01-01T00:00:00Z\"}"
      (eitherDecode json :: Either String Workout) `shouldSatisfy` isLeft

  describe "WorkoutsResponse JSON parsing" $ do
    it "parses valid WorkoutsResponse JSON" $ do
      let json = LBS.pack "{\"workouts\":[{\"id\":\"1\",\"title\":\"Morning Run\",\"created_at\":\"2023-01-01T00:00:00Z\",\"start_time\":null,\"exercises\":[]}],\"page_count\":1}"
      let expected = WorkoutsResponse [Workout (pack "1") (pack "Morning Run") (pack "2023-01-01T00:00:00Z") Nothing []] 1
      eitherDecode json `shouldBe` Right expected
    it "parses empty workouts list" $ do
      let json = LBS.pack "{\"workouts\":[],\"page_count\":0}"
      eitherDecode json `shouldBe` Right (WorkoutsResponse [] 0)
    it "fails on invalid page_count type" $ do
      let json = LBS.pack "{\"workouts\":[],\"page_count\":\"invalid\"}"
      (eitherDecode json :: Either String WorkoutsResponse) `shouldSatisfy` isLeft
{-# LANGUAGE OverloadedStrings #-}

module CoreSpec (spec) where

import Test.Hspec
import Data.ByteString.Lazy.Char8 as BS (pack)
import Data.Time (parseTimeM, defaultTimeLocale, UTCTime)
import Lib.Core
import Hevy (Workout(..), WorkoutsResponse(..))
import Data.Text as Text (pack)
import Data.Maybe (fromMaybe, isJust)
import Data.Aeson (encode, object, (.=))
import qualified Data.Vector as V

spec :: Spec
spec = do
  describe "parseWorkoutsResponse" $ do
    it "parses valid JSON response" $ do
      let json = BS.pack "{\"workouts\": [{\"id\": \"1\", \"title\": \"Test\", \"created_at\": \"2023-01-01T00:00:00Z\", \"start_time\": null, \"exercises\": []}], \"page_count\": 1}"
      let expected = WorkoutsResponse [Workout "1" "Test" "2023-01-01T00:00:00Z" Nothing []] 1
      case parseWorkoutsResponse json of
        Left err -> expectationFailure err
        Right (resp, ws) -> do
          resp `shouldBe` expected
          ws `shouldBe` V.fromList (workouts resp)
    it "fails on invalid JSON" $ do
      let json = BS.pack "invalid json"
      parseWorkoutsResponse json `shouldSatisfy` (\x -> case x of Left _ -> True; Right _ -> False)

  describe "calculateTotalPages" $ do
    it "returns page_count if greater than 1" $ do
      let resp = WorkoutsResponse [] 5
      calculateTotalPages resp `shouldBe` 5
    it "returns 1 if page_count is 0" $ do
      let resp = WorkoutsResponse [] 0
      calculateTotalPages resp `shouldBe` 1
    it "returns 1 if page_count is negative" $ do
      let resp = WorkoutsResponse [] (-1)
      calculateTotalPages resp `shouldBe` 1

  describe "combinePageResults" $ do
    it "combines two successful results" $ do
      let r1 = Right (V.fromList [Workout (Text.pack "1") (Text.pack "Run") (Text.pack "2023-01-01T00:00:00Z") Nothing []])
      let r2 = Right (V.fromList [Workout (Text.pack "2") (Text.pack "Lift") (Text.pack "2023-01-02T00:00:00Z") Nothing []])
      combinePageResults r1 r2 `shouldBe` Right (V.fromList [Workout (Text.pack "1") (Text.pack "Run") (Text.pack "2023-01-01T00:00:00Z") Nothing [], Workout (Text.pack "2") (Text.pack "Lift") (Text.pack "2023-01-02T00:00:00Z") Nothing []])
    it "returns Left if first argument is Left" $ do
      let r1 = Left "error1"
      let r2 = Right V.empty
      combinePageResults r1 r2 `shouldBe` Left "error1"
    it "returns Left if second argument is Left" $ do
      let r1 = Right V.empty
      let r2 = Left "error2"
      combinePageResults r1 r2 `shouldBe` Left "error2"
    it "combines empty lists" $ do
      let r1 = Right V.empty
      let r2 = Right V.empty
      combinePageResults r1 r2 `shouldBe` Right V.empty

  describe "combineAllPages" $ do
    it "combines multiple pages" $ do
      let initial = V.fromList [Workout (Text.pack "1") (Text.pack "Run") (Text.pack "2023-01-01T00:00:00Z") Nothing []]
      let others = [Right (V.fromList [Workout (Text.pack "2") (Text.pack "Lift") (Text.pack "2023-01-02T00:00:00Z") Nothing []]), Right (V.fromList [Workout (Text.pack "3") (Text.pack "Swim") (Text.pack "2023-01-03T00:00:00Z") Nothing []])]
      combineAllPages initial others `shouldBe` Right (V.fromList [Workout (Text.pack "1") (Text.pack "Run") (Text.pack "2023-01-01T00:00:00Z") Nothing [], Workout (Text.pack "2") (Text.pack "Lift") (Text.pack "2023-01-02T00:00:00Z") Nothing [], Workout (Text.pack "3") (Text.pack "Swim") (Text.pack "2023-01-03T00:00:00Z") Nothing []])
    it "stops at first error" $ do
      let initial = V.fromList [Workout (Text.pack "1") (Text.pack "Run") (Text.pack "2023-01-01T00:00:00Z") Nothing []]
      let others = [Right (V.fromList [Workout (Text.pack "2") (Text.pack "Lift") (Text.pack "2023-01-02T00:00:00Z") Nothing []]), Left "error"]
      combineAllPages initial others `shouldBe` Left "error"
    it "handles empty initial list" $ do
      let initial = V.empty
      let others = [Right (V.fromList [Workout (Text.pack "1") (Text.pack "Run") (Text.pack "2023-01-01T00:00:00Z") Nothing []])]
      combineAllPages initial others `shouldBe` Right (V.fromList [Workout (Text.pack "1") (Text.pack "Run") (Text.pack "2023-01-01T00:00:00Z") Nothing []])

  describe "parseDate" $ do
    it "parses ISO 8601 with timezone" $ do
      let result = parseDate "2023-01-01T12:00:00Z"
      result `shouldSatisfy` isJust
      let expected = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Z" "2023-01-01T12:00:00Z" :: Maybe UTCTime
      result `shouldBe` expected
    it "returns Nothing for invalid date" $ do
      parseDate "invalid" `shouldBe` Nothing

  describe "groupWorkoutsByWeeks" $ do
    it "groups workouts by week correctly" $ do
      let w1 = Workout "1" "W1" "2023-01-01" (Just "2023-01-01T12:00:00Z") []
      let w2 = Workout "2" "W2" "2023-01-02" (Just "2023-01-02T12:00:00Z") []
      let w3 = Workout "3" "W3" "2023-01-08" (Just "2023-01-08T12:00:00Z") []
      let workouts = V.fromList [w1, w2, w3]
      let groups = groupWorkoutsByWeeks Nothing workouts
      length groups `shouldBe` 4
      groups `shouldBe` [[w1, w2], [w3], [], []]

  describe "formatWorkoutsOutput" $ do
    it "formats workouts correctly" $ do
      let w1 = Workout "1" "Run" "2023-01-01" (Just "2023-01-01T12:00:00Z") []
      let groups = [[w1]]
      let result = formatWorkoutsOutput Nothing groups
      let expected = encode [object ["week" .= (1 :: Int), "workouts" .= [object ["name" .= ("Run" :: String), "start_time" .= ("2023-01-01T12:00:00Z" :: String), "exercises" .= ([] :: [String])]]]]
      result `shouldBe` expected
module LibSpec (spec) where

import Test.Hspec
import Data.Aeson (encode)
import Data.ByteString.Lazy (ByteString)
import Data.Time (UTCTime, parseTimeM, defaultTimeLocale)  -- Remove addDays e utctDay
import Data.Text (pack)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Hevy (Workout(..), WorkoutsResponse(..))
import Lib.Core

spec :: Spec
spec = do
  describe "parseWorkoutsResponse" $ do
    it "parses a valid WorkoutsResponse" $ do
      let input = encode $ WorkoutsResponse [Workout (pack "1") (pack "Run") (pack "2023-01-01T00:00:00Z")] 1
      parseWorkoutsResponse input `shouldBe` Right (WorkoutsResponse [Workout (pack "1") (pack "Run") (pack "2023-01-01T00:00:00Z")] 1, [Workout (pack "1") (pack "Run") (pack "2023-01-01T00:00:00Z")])

    it "fails on invalid JSON" $ do
      let input = LBS.pack "invalid json" :: ByteString
      parseWorkoutsResponse input `shouldSatisfy` (\x -> case x of Left _ -> True; Right _ -> False)

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
      let r1 = Right [Workout (pack "1") (pack "Run") (pack "2023-01-01T00:00:00Z")]
      let r2 = Right [Workout (pack "2") (pack "Lift") (pack "2023-01-02T00:00:00Z")]
      combinePageResults r1 r2 `shouldBe` Right [Workout (pack "1") (pack "Run") (pack "2023-01-01T00:00:00Z"), Workout (pack "2") (pack "Lift") (pack "2023-01-02T00:00:00Z")]

    it "returns Left if first argument is Left" $ do
      let r1 = Left "error1"
      let r2 = Right []
      combinePageResults r1 r2 `shouldBe` Left "error1"

    it "returns Left if second argument is Left" $ do
      let r1 = Right []
      let r2 = Left "error2"
      combinePageResults r1 r2 `shouldBe` Left "error2"

    it "combines empty lists" $ do
      let r1 = Right []
      let r2 = Right []
      combinePageResults r1 r2 `shouldBe` Right []

  describe "combineAllPages" $ do
    it "combines multiple pages" $ do
      let initial = [Workout (pack "1") (pack "Run") (pack "2023-01-01T00:00:00Z")]
      let others = [Right [Workout (pack "2") (pack "Lift") (pack "2023-01-02T00:00:00Z")], Right [Workout (pack "3") (pack "Swim") (pack "2023-01-03T00:00:00Z")]]
      combineAllPages initial others `shouldBe` Right [Workout (pack "1") (pack "Run") (pack "2023-01-01T00:00:00Z"), Workout (pack "2") (pack "Lift") (pack "2023-01-02T00:00:00Z"), Workout (pack "3") (pack "Swim") (pack "2023-01-03T00:00:00Z")]

    it "stops at first error" $ do
      let initial = [Workout (pack "1") (pack "Run") (pack "2023-01-01T00:00:00Z")]
      let others = [Right [Workout (pack "2") (pack "Lift") (pack "2023-01-02T00:00:00Z")], Left "error"]
      combineAllPages initial others `shouldBe` Left "error"

    it "handles empty initial list" $ do
      let initial = []
      let others = [Right [Workout (pack "1") (pack "Run") (pack "2023-01-01T00:00:00Z")]]
      combineAllPages initial others `shouldBe` Right [Workout (pack "1") (pack "Run") (pack "2023-01-01T00:00:00Z")]

  describe "parseDate" $ do
    it "parses a valid date string" $ do
      let result = parseDate "2023-01-01T00:00:00Z"
      let expected = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" "2023-01-01T00:00:00Z" :: Maybe UTCTime
      result `shouldBe` expected

    it "returns Nothing for invalid date" $ do
      parseDate "invalid-date" `shouldBe` Nothing

  describe "groupWorkoutsByWeeks" $ do
    it "groups workouts within 28 days" $ do
      let w1 = Workout (pack "1") (pack "Run") (pack "2023-01-01T00:00:00Z")
      let w2 = Workout (pack "2") (pack "Lift") (pack "2023-01-15T00:00:00Z")
      let w3 = Workout (pack "3") (pack "Swim") (pack "2023-02-01T00:00:00Z")
      groupWorkoutsByWeeks [w1, w2, w3] `shouldBe` [[w1, w2], [w3]]

    it "handles invalid dates by not grouping" $ do
      let w1 = Workout (pack "1") (pack "Run") (pack "invalid-date")
      let w2 = Workout (pack "2") (pack "Lift") (pack "2023-01-15T00:00:00Z")
      groupWorkoutsByWeeks [w1, w2] `shouldBe` [[w1], [w2]]

    it "handles empty list" $ do
      groupWorkoutsByWeeks [] `shouldBe` []

  describe "filterWorkoutsByStartDate" $ do
    it "filters workouts before start date" $ do
      let startDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" "2023-01-10" :: Maybe UTCTime
      let w1 = Workout (pack "1") (pack "Run") (pack "2023-01-01T00:00:00Z")
      let w2 = Workout (pack "2") (pack "Lift") (pack "2023-01-15T00:00:00Z")
      let groups = [[w1], [w2]]
      filterWorkoutsByStartDate startDate groups `shouldBe` [[w2]]

    it "returns all groups if no start date" $ do
      let groups = [[Workout (pack "1") (pack "Run") (pack "2023-01-01T00:00:00Z")]]
      filterWorkoutsByStartDate Nothing groups `shouldBe` groups

    it "handles empty groups" $ do
      let startDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" "2023-01-10" :: Maybe UTCTime
      filterWorkoutsByStartDate startDate [] `shouldBe` []

    it "filters out invalid dates" $ do
      let startDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" "2023-01-10" :: Maybe UTCTime
      let w1 = Workout (pack "1") (pack "Run") (pack "invalid-date")
      let w2 = Workout (pack "2") (pack "Lift") (pack "2023-01-15T00:00:00Z")
      let groups = [[w1, w2]]
      filterWorkoutsByStartDate startDate groups `shouldBe` [[w2]]
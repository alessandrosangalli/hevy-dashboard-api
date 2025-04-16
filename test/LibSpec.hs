module LibSpec (spec) where

import Test.Hspec
import Data.Aeson (encode)
import Data.ByteString.Lazy (ByteString)
import Data.Time (parseTimeM, defaultTimeLocale, UTCTime)
import Data.Text (pack)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Hevy (Workout(..), WorkoutsResponse(..))
import Lib.Core
import qualified Data.Vector as V

spec :: Spec
spec = do
  describe "parseWorkoutsResponse" $ do
    it "parses a valid WorkoutsResponse" $ do
      let input = encode $ WorkoutsResponse [Workout (pack "1") (pack "Run") (pack "2023-01-01T00:00:00Z") Nothing []] 1
      parseWorkoutsResponse input `shouldBe` Right (WorkoutsResponse [Workout (pack "1") (pack "Run") (pack "2023-01-01T00:00:00Z") Nothing []] 1, V.fromList [Workout (pack "1") (pack "Run") (pack "2023-01-01T00:00:00Z") Nothing []])
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
      let r1 = Right (V.fromList [Workout (pack "1") (pack "Run") (pack "2023-01-01T00:00:00Z") Nothing []])
      let r2 = Right (V.fromList [Workout (pack "2") (pack "Lift") (pack "2023-01-02T00:00:00Z") Nothing []])
      combinePageResults r1 r2 `shouldBe` Right (V.fromList [Workout (pack "1") (pack "Run") (pack "2023-01-01T00:00:00Z") Nothing [], Workout (pack "2") (pack "Lift") (pack "2023-01-02T00:00:00Z") Nothing []])
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
      let initial = V.fromList [Workout (pack "1") (pack "Run") (pack "2023-01-01T00:00:00Z") Nothing []]
      let others = [Right (V.fromList [Workout (pack "2") (pack "Lift") (pack "2023-01-02T00:00:00Z") Nothing []]), Right (V.fromList [Workout (pack "3") (pack "Swim") (pack "2023-01-03T00:00:00Z") Nothing []])]
      combineAllPages initial others `shouldBe` Right (V.fromList [Workout (pack "1") (pack "Run") (pack "2023-01-01T00:00:00Z") Nothing [], Workout (pack "2") (pack "Lift") (pack "2023-01-02T00:00:00Z") Nothing [], Workout (pack "3") (pack "Swim") (pack "2023-01-03T00:00:00Z") Nothing []])
    it "stops at first error" $ do
      let initial = V.fromList [Workout (pack "1") (pack "Run") (pack "2023-01-01T00:00:00Z") Nothing []]
      let others = [Right (V.fromList [Workout (pack "2") (pack "Lift") (pack "2023-01-02T00:00:00Z") Nothing []]), Left "error"]
      combineAllPages initial others `shouldBe` Left "error"
    it "handles empty initial list" $ do
      let initial = V.empty
      let others = [Right (V.fromList [Workout (pack "1") (pack "Run") (pack "2023-01-01T00:00:00Z") Nothing []])]
      combineAllPages initial others `shouldBe` Right (V.fromList [Workout (pack "1") (pack "Run") (pack "2023-01-01T00:00:00Z") Nothing []])

  describe "parseDate" $ do
    it "parses a valid date string" $ do
      let result = parseDate "2023-01-01T00:00:00Z"
      let expected = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Z" "2023-01-01T00:00:00Z" :: Maybe UTCTime
      result `shouldBe` expected
    it "returns Nothing for invalid date" $ do
      parseDate "invalid-date" `shouldBe` Nothing

  describe "groupWorkoutsByWeeks" $ do
    it "groups workouts within 28 days" $ do
      let w1 = Workout (pack "1") (pack "Run") (pack "2023-01-01T00:00:00Z") (Just (pack "2023-01-01T00:00:00Z")) []
      let w2 = Workout (pack "2") (pack "Lift") (pack "2023-01-15T00:00:00Z") (Just (pack "2023-01-15T00:00:00Z")) []
      let w3 = Workout (pack "3") (pack "Swim") (pack "2023-02-01T00:00:00Z") (Just (pack "2023-02-01T00:00:00Z")) []
      let workouts = V.fromList [w1, w2, w3]
      groupWorkoutsByWeeks Nothing workouts `shouldBe` [[w1], [w2], [w3], []]
    it "handles invalid dates by not grouping" $ do
      let w1 = Workout (pack "1") (pack "Run") (pack "invalid-date") Nothing []
      let w2 = Workout (pack "2") (pack "Lift") (pack "2023-01-15T00:00:00Z") (Just (pack "2023-01-15T00:00:00Z")) []
      let workouts = V.fromList [w1, w2]
      groupWorkoutsByWeeks Nothing workouts `shouldBe` [[w1], [w2], [], []]
    it "handles empty list" $ do
      groupWorkoutsByWeeks Nothing V.empty `shouldBe` [[], [], [], []]
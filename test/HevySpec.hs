module HevySpec (spec) where

import Test.Hspec
import Data.Aeson (encode, eitherDecode)
import Data.Either (isLeft)
import Data.Text (Text, pack)
import qualified Data.ByteString.Lazy.Char8 as LBS  -- Para converter String em ByteString
import Hevy (Workout(..), WorkoutsResponse(..))

spec :: Spec
spec = do
  describe "Workout JSON" $ do
    it "parses a valid Workout JSON" $ do
      let json = LBS.pack "{\"id\":\"1\",\"title\":\"Morning Run\",\"created_at\":\"2023-01-01T00:00:00Z\"}"
      eitherDecode json `shouldBe` Right (Workout (pack "1") (pack "Morning Run") (pack "2023-01-01T00:00:00Z"))

    it "fails on missing required fields" $ do
      let json = LBS.pack "{\"id\":\"1\",\"title\":\"Morning Run\"}" -- missing created_at
      (eitherDecode json :: Either String Workout) `shouldSatisfy` isLeft

    it "fails on invalid JSON structure" $ do
      let json = LBS.pack "{\"id\":1,\"title\":\"Morning Run\",\"created_at\":\"2023-01-01T00:00:00Z\"}" -- id should be a string
      (eitherDecode json :: Either String Workout) `shouldSatisfy` isLeft

  describe "WorkoutsResponse JSON" $ do
    it "parses a valid WorkoutsResponse JSON" $ do
      let json = LBS.pack "{\"workouts\":[{\"id\":\"1\",\"title\":\"Morning Run\",\"created_at\":\"2023-01-01T00:00:00Z\"}],\"page_count\":1}"
      let expected = WorkoutsResponse [Workout (pack "1") (pack "Morning Run") (pack "2023-01-01T00:00:00Z")] 1
      eitherDecode json `shouldBe` Right expected

    it "parses empty workouts list" $ do
      let json = LBS.pack "{\"workouts\":[],\"page_count\":0}"
      eitherDecode json `shouldBe` Right (WorkoutsResponse [] 0)

    it "fails on invalid page_count type" $ do
      let json = LBS.pack "{\"workouts\":[],\"page_count\":\"invalid\"}" -- page_count should be an int
      (eitherDecode json :: Either String WorkoutsResponse) `shouldSatisfy` isLeft
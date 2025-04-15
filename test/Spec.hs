{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import Lib (fetchAllWorkoutsWithConfigAndLog, defaultConfig, HttpRequest)
import Data.Either (isRight, isLeft)
import System.Environment (setEnv)
import Network.HTTP.Client (Request, Manager, Response)
import Network.HTTP.Types (Status, status200, ResponseHeaders, http11)
import Network.HTTP.Client.Internal (Response(..), CookieJar(CJ))
import Data.ByteString.Lazy (ByteString)
import Hevy (WorkoutsResponse(..), Workout(..))
import Data.Aeson (encode, object, (.=), Object)

main :: IO ()
main = do
  setEnv "HEVY_API_KEY" "test-api-key"
  hspec $ do
    describe "Lib" $ do
      it "fetchAllWorkouts should return a Right value with mocked results" $ do
        let mockHttpRequest :: HttpRequest
            mockHttpRequest _ _ _ = do
              let mockBody = encode $ object
                    [ "workouts" .= [object
                        [ "id" .= ("1" :: String)
                        , "title" .= ("Run" :: String)
                        , "created_at" .= ("2023-01-01T00:00:00Z" :: String)
                        ]]
                    , "page_count" .= (1 :: Int)
                    ]
              pure $ Right $ makeResponse mockBody
        result <- fetchAllWorkoutsWithConfigAndLog defaultConfig Nothing (const $ pure ()) mockHttpRequest
        result `shouldBe` Right [Workout "1" "Run" "2023-01-01T00:00:00Z"]

      it "fetchAllWorkouts should handle an empty workouts list" $ do
        let mockHttpRequest :: HttpRequest
            mockHttpRequest _ _ _ = do
              let mockBody = encode $ object
                    [ "workouts" .= ([] :: [Object])
                    , "page_count" .= (1 :: Int)
                    ]
              pure $ Right $ makeResponse mockBody
        result <- fetchAllWorkoutsWithConfigAndLog defaultConfig Nothing (const $ pure ()) mockHttpRequest
        result `shouldBe` Right []

      it "fetchAllWorkouts should handle an error response" $ do
        let mockHttpRequest :: HttpRequest
            mockHttpRequest _ _ _ = pure $ Left "Error fetching workouts"
        result <- fetchAllWorkoutsWithConfigAndLog defaultConfig Nothing (const $ pure ()) mockHttpRequest
        result `shouldSatisfy` isLeft

-- Função auxiliar para criar uma resposta mockada
makeResponse :: ByteString -> Response ByteString
makeResponse body = Response
  { responseStatus = status200
  , responseVersion = http11
  , responseHeaders = []
  , responseBody = body
  , responseCookieJar = CJ []
  }
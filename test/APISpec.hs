{-# LANGUAGE OverloadedStrings #-}

module APISpec (spec) where

import Test.Hspec
import Lib.API
import Lib.Types (FetchConfig(..), defaultConfig, HttpRequest)
import Data.Either (isRight)
import System.Environment (setEnv, unsetEnv)
import Network.HTTP.Client (Request, Manager, Response, queryString)
import Network.HTTP.Client.Internal (Response(..), CookieJar(CJ))
import Network.HTTP.Types (status200, http11, parseQuery)
import Data.ByteString.Lazy (ByteString)
import Hevy (WorkoutsResponse(..), Workout(..))
import Data.Aeson (encode, object, (.=))
import Data.Aeson.Key (fromString)
import qualified Data.ByteString.Char8 as BS
import Data.Text (pack)
import qualified Data.Vector as V

spec :: Spec
spec = do
  describe "fetchAllWorkoutsWithConfigAndLog" $ do
    before (setEnv "HEVY_API_KEY" "test-api-key") $
      after (\_ -> unsetEnv "HEVY_API_KEY") $
        do
          it "returns Right with mocked results (single page)" $ do
            let mockHttpRequest :: HttpRequest
                mockHttpRequest _ _ _ = do
                  let mockBody = encode $ object
                        [ fromString "workouts" .= [object
                            [ fromString "id" .= ("1" :: String)
                            , fromString "title" .= ("Run" :: String)
                            , fromString "created_at" .= ("2023-01-01T00:00:00Z" :: String)
                            , fromString "start_time" .= (Nothing :: Maybe String)
                            , fromString "exercises" .= ([] :: [String])
                            ]]
                        , fromString "page_count" .= (1 :: Int)
                        ]
                  pure $ Right $ makeResponse mockBody
            config <- defaultConfig
            result <- fetchAllWorkoutsWithConfigAndLog config Nothing (const $ pure ()) mockHttpRequest
            result `shouldBe` Right (V.fromList [Workout (pack "1") (pack "Run") (pack "2023-01-01T00:00:00Z") Nothing []])

          it "handles multiple pages" $ do
            let pageCount = 2 :: Int
            let mockHttpRequest :: HttpRequest
                mockHttpRequest req _ _ = do
                  let qs = parseQuery (queryString req)
                  let page = case filter (\(k, _) -> k == "page") qs of
                               [(_, Just v)] -> read (BS.unpack v) :: Int
                               _ -> 1
                  let mockBody = encode $ object
                        [ fromString "workouts" .= [object
                            [ fromString "id" .= (show page :: String)
                            , fromString "title" .= ("Run " ++ show page :: String)
                            , fromString "created_at" .= ("2023-01-0" ++ show page ++ "T00:00:00Z" :: String)
                            , fromString "start_time" .= (Nothing :: Maybe String)
                            , fromString "exercises" .= ([] :: [String])
                            ]]
                        , fromString "page_count" .= pageCount
                        ]
                  pure $ Right $ makeResponse mockBody
            config <- defaultConfig
            result <- fetchAllWorkoutsWithConfigAndLog config Nothing (const $ pure ()) mockHttpRequest
            result `shouldBe` Right (V.fromList [Workout (pack "1") (pack "Run 1") (pack "2023-01-01T00:00:00Z") Nothing [], Workout (pack "2") (pack "Run 2") (pack "2023-01-02T00:00:00Z") Nothing []])

          it "fails without API key" $ do
            unsetEnv "HEVY_API_KEY"
            let mockHttpRequest :: HttpRequest
                mockHttpRequest _ _ _ = error "Should not reach HTTP request without API key"
            config <- defaultConfig
            result <- fetchAllWorkoutsWithConfigAndLog config Nothing (const $ pure ()) mockHttpRequest
            result `shouldBe` Left "HEVY_API_KEY not set."

  describe "fetchAllWorkouts" $ do
    it "uses default config" $ do
      setEnv "HEVY_API_KEY" "test-api-key"
      let mockHttpRequest :: HttpRequest
          mockHttpRequest _ _ _ = pure $ Right $ makeResponse $ encode $ WorkoutsResponse [] 1
      config <- defaultConfig
      result <- fetchAllWorkoutsWithConfigAndLog config Nothing (const $ pure ()) mockHttpRequest
      result `shouldSatisfy` isRight

makeResponse :: ByteString -> Response ByteString
makeResponse body = Response
  { responseStatus = status200
  , responseVersion = http11
  , responseHeaders = []
  , responseBody = body
  , responseCookieJar = CJ []
  }
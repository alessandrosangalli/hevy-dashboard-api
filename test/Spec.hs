{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import Lib.API (fetchAllWorkoutsWithConfigAndLog)
import Lib.Types (defaultConfig, HttpRequest)
import Data.Either (isRight)
import System.Environment (setEnv, unsetEnv)
import Network.HTTP.Client (Request, Manager, Response, queryString)
import Network.HTTP.Client.Internal (Response(..), CookieJar(CJ))
import Network.HTTP.Types (Status, status200, ResponseHeaders, http11, parseQuery)
import Data.ByteString.Lazy (ByteString)
import Hevy (WorkoutsResponse(..), Workout(..))
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Char8 as BS
import Data.Text (pack)

main :: IO ()
main = do
  setEnv "HEVY_API_KEY" "test-api-key"
  hspec $ do
    describe "Lib.API" $ do
      it "fetchAllWorkoutsWithConfigAndLog returns a Right value with mocked results (single page)" $ do
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
        result `shouldBe` Right [Workout (pack "1") (pack "Run") (pack "2023-01-01T00:00:00Z")]

      it "fetchAllWorkoutsWithConfigAndLog handles multiple pages" $ do
        let pageCount = 2 :: Int
        let mockHttpRequest :: HttpRequest
            mockHttpRequest req _ _ = do
              let qs = parseQuery (queryString req)
              let page = case filter (\(k, _) -> k == "page") qs of
                           [(_, Just v)] -> read (BS.unpack v) :: Int
                           _ -> 1  -- Valor padrão se "page" não for encontrado ou for inválido
              let mockBody = encode $ object
                    [ "workouts" .= [object
                        [ "id" .= (show page :: String)
                        , "title" .= ("Run " ++ show page :: String)
                        , "created_at" .= ("2023-01-0" ++ show page ++ "T00:00:00Z" :: String)
                        ]]
                    , "page_count" .= pageCount
                    ]
              pure $ Right $ makeResponse mockBody
        result <- fetchAllWorkoutsWithConfigAndLog defaultConfig Nothing (const $ pure ()) mockHttpRequest
        result `shouldBe` Right [Workout (pack "1") (pack "Run 1") (pack "2023-01-01T00:00:00Z"), Workout (pack "2") (pack "Run 2") (pack "2023-01-02T00:00:00Z")]

      it "fetchAllWorkoutsWithConfigAndLog fails without API key" $ do
        unsetEnv "HEVY_API_KEY"
        let mockHttpRequest :: HttpRequest
            mockHttpRequest _ _ _ = error "Should not reach HTTP request without API key"
        result <- fetchAllWorkoutsWithConfigAndLog defaultConfig Nothing (const $ pure ()) mockHttpRequest
        result `shouldBe` Left "The environment variable HEVY_API_KEY is not defined."

makeResponse :: ByteString -> Response ByteString
makeResponse body = Response
  { responseStatus = status200
  , responseVersion = http11
  , responseHeaders = []
  , responseBody = body
  , responseCookieJar = CJ []
  }
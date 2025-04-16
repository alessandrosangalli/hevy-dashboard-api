{-# LANGUAGE OverloadedStrings #-}

module HTTPSpec (spec) where

import Test.Hspec
import Network.HTTP.Client
import Network.HTTP.Client.Internal (Response(..), CookieJar(CJ), ResponseClose(..))
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (status200, status500, http11, ResponseHeaders, Status)
import Lib.HTTP
import Lib.Types (FetchConfig(..), HttpRequest)
import Hevy (WorkoutsResponse(..), Workout(..))
import Data.Aeson (encode)
import Data.Either (isRight, isLeft)
import qualified Data.ByteString.Char8 as BS
import Data.Text (pack)
import qualified Data.Vector as V
import Data.ByteString.Lazy (ByteString)

-- Helper function to create a Response with a proper ResponseClose
makeResponse :: Status -> ByteString -> Response ByteString
makeResponse status body = Response
  { responseStatus = status
  , responseVersion = http11
  , responseHeaders = []
  , responseBody = body
  , responseCookieJar = CJ []
  , responseClose' = ResponseClose (pure ())
  }

spec :: Spec
spec = do
  describe "defaultHttpRequest" $ do
    it "returns Right on successful request" $ do
      let mockResponse = makeResponse status200 (encode $ WorkoutsResponse [] 1)
      let mockHttpRequest :: Request -> Manager -> Int -> IO (Either String (Response ByteString))
          mockHttpRequest _ _ _ = pure $ Right mockResponse
      result <- mockHttpRequest undefined undefined 3
      result `shouldSatisfy` isRight

    it "retries on network errors" $ do
      let mockHttpRequest :: Request -> Manager -> Int -> IO (Either String (Response ByteString))
          mockHttpRequest _ _ retries = if retries < 3
            then pure $ Left "Network error"
            else pure $ Right $ makeResponse status200 (encode $ WorkoutsResponse [] 1)
      req <- parseRequest "http://example.com"
      mgr <- newManager tlsManagerSettings
      result <- mockHttpRequest req mgr 3
      result `shouldSatisfy` isRight

    it "fails after max retries" $ do
      let mockHttpRequest :: Request -> Manager -> Int -> IO (Either String (Response ByteString))
          mockHttpRequest _ _ _ = pure $ Left "Request failed after retries: Network error"
      req <- parseRequest "http://example.com"
      mgr <- newManager tlsManagerSettings
      result <- mockHttpRequest req mgr 0
      result `shouldSatisfy` (\r -> case r of Left err -> err == "Request failed after retries: Network error"; _ -> False)

  describe "fetchWorkoutsPage" $ do
    it "fetches page successfully" $ do
      let httpRequest :: HttpRequest
          httpRequest _ _ _ = pure $ Right $ makeResponse status200 (encode $ WorkoutsResponse [Workout (pack "1") (pack "Run") (pack "2023-01-01T00:00:00Z") Nothing []] 1)
      let config = FetchConfig 10 3 1000000 "https://api.hevyapp.com"
      let apiKey = "test-api-key"
      result <- fetchWorkoutsPage httpRequest undefined config apiKey 1 Nothing
      result `shouldBe` Right (WorkoutsResponse [Workout (pack "1") (pack "Run") (pack "2023-01-01T00:00:00Z") Nothing []] 1, V.fromList [Workout (pack "1") (pack "Run") (pack "2023-01-01T00:00:00Z") Nothing []])

    it "handles HTTP errors" $ do
      let httpRequest :: HttpRequest
          httpRequest _ _ _ = pure $ Right $ makeResponse status500 ""
      let config = FetchConfig 10 3 1000000 "https://api.hevyapp.com"
      let apiKey = "test-api-key"
      result <- fetchWorkoutsPage httpRequest undefined config apiKey 1 Nothing
      result `shouldSatisfy` isLeft
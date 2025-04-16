module TypesSpec (spec) where

import Test.Hspec
import Lib.Types
import System.Environment (setEnv, unsetEnv)

spec :: Spec
spec = do
  describe "defaultConfig" $ do
    it "uses default values when HEVY_API_URL is not set" $ do
      unsetEnv "HEVY_API_URL"
      config <- defaultConfig
      fcPageSize config `shouldBe` 10
      fcRetries config `shouldBe` 3
      fcDelay config `shouldBe` 1000000
      fcBaseUrl config `shouldBe` "https://api.hevyapp.com"

    it "uses HEVY_API_URL when set" $ do
      setEnv "HEVY_API_URL" "https://custom.api.com"
      config <- defaultConfig
      fcBaseUrl config `shouldBe` "https://custom.api.com"
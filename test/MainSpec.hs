module Main (main) where

import Test.Hspec
import qualified HevySpec
import qualified APISpec
import qualified CoreSpec
import qualified HTTPSpec
import qualified TypesSpec

main :: IO ()
main = hspec $ do
  HevySpec.spec
  APISpec.spec
  CoreSpec.spec
  HTTPSpec.spec
  TypesSpec.spec
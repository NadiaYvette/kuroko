module Main (main) where

import Test.Hspec
import qualified MockSpec
import qualified MCPSpec
import qualified StoreSpec
import qualified DocRecordSpec
import qualified LouterIntegrationSpec
import qualified PolicySpec

main :: IO ()
main = hspec $ do
  MockSpec.spec
  MCPSpec.spec
  StoreSpec.spec
  DocRecordSpec.spec
  LouterIntegrationSpec.spec
  PolicySpec.spec

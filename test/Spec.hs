module Main (main) where

import Test.Hspec
import qualified MockSpec

main :: IO ()
main = hspec $ do
  MockSpec.spec

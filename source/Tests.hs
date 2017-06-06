module Tests where

import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord


main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Simple testing unit test" $
    [1,2,3] `compare` [1,2] @?= GT
  ]

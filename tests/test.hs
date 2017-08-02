import Test.Tasty
import Test.Tasty.HUnit

import Text.ParserCombinators.Parsec
import NamedLambda


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parserTests]

parserTests :: TestTree
parserTests = testGroup "Unit tests"
  [ testCase "INR parser test" $
    parse lambdaexp "" "INR (\\x.y)"
    @?=
    Right (TypedInr (LambdaAbstraction "x" (LambdaVariable "y")))

  , testCase "INL parser test" $
    parse lambdaexp "" "INL (\\ab.cd)"
    @?=
    Right (TypedInl (LambdaAbstraction "ab" (LambdaVariable "cd")))
  ]

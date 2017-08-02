import Test.Tasty
import Test.Tasty.HUnit

import Text.ParserCombinators.Parsec
import NamedLambda
import Lambda
import Types
import Ski


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ parserTests
  , typeinferTests
  , skiabsTests 
  ]

parserTests :: TestTree
parserTests = testGroup "Parser tests"
  [ testCase "INR parser test" $
    parse lambdaexp "" "INR (\\x.y)"
    @?=
    Right (TypedInr (LambdaAbstraction "x" (LambdaVariable "y")))

  , testCase "INL parser test" $
    parse lambdaexp "" "INL (\\ab.cd)"
    @?=
    Right (TypedInl (LambdaAbstraction "ab" (LambdaVariable "cd")))

  , testCase "Spaces between lambdas test" $
    parse lambdaexp "" "(\\x.      x)"
    @?=
    Right (LambdaAbstraction "x" (LambdaVariable "x"))

  , testCase "Multiple-character variable name test" $
    parse lambdaexp "" "(\\asdf. asdf)"
    @?=
    Right (LambdaAbstraction "asdf" (LambdaVariable "asdf"))
  ]

typeinferTests :: TestTree
typeinferTests = testGroup "Type inference tests"
  [ testCase "Identity type inference" $
    typeinference (Lambda (Var 1))
    @?=
    Just (Arrow (Tvar 0) (Tvar 0))

  , testCase "Double negation of LEM" $
    typeinference (Lambda (Absurd ((App (Var 1) (Inr (Lambda (App (Var 2) (Inl (Var 1)))))))))
    @?=
    Just (Arrow (Arrow (Union (Tvar 0) (Arrow (Tvar 0) Bottom)) Bottom) Bottom)
  ]

skiabsTests :: TestTree
skiabsTests = testGroup "SKI abstraction tests"
  [ testCase "Identity SKI abstraction" $
    skiabs (LambdaAbstraction "x" (LambdaVariable "x"))
    @?=
    I

  , testCase "Numeral 2 SKI abstraction" $
    skiabs (LambdaAbstraction "a"
            (LambdaAbstraction "b"
              (LambdaApplication (LambdaVariable "a")
                (LambdaApplication (LambdaVariable "a") (LambdaVariable "b")))))
    @?=
    Comb (Comb S (Comb (Comb S (Comb K S)) K)) I
  ]

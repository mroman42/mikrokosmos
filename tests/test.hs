import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Text.ParserCombinators.Parsec
import NamedLambda
import Lambda
import Types
import Ski
import Environment


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ parserTests
  , typeinferTests
  , skiabsTests
  , lambdaProps
  ]

-- Unit tests
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


-- Lambda properties
lambdaProps :: TestTree
lambdaProps = testGroup "Lambda expression properties (quickcheck)"
  [ QC.testProperty "expression -> named -> expression" $
      \exp -> toBruijn emptyContext (nameExp exp) == exp
  , QC.testProperty "open expressions not allowed" $
      \exp -> isOpenExp exp == False
  ]
  

-- Arbitrary untyped lambda expressions
-- {-# LANGUAGE TypeSynonymInstances #-}
-- type UntypedExp = Exp
-- instance Arbitrary UntypedExp where
--   arbitrary = sized (untlambda 0)

-- untlambda :: Int -> Int -> Gen UntypedExp
-- untlambda 0   0    = return $ Lambda (Var 1)
-- untlambda 0   size = Lambda <$> untlambda 1 (size-1)
-- untlambda lim 0    = Var <$> (toInteger <$> (choose (1, lim)))
-- untlambda lim size = oneof
--   [ Var <$> (toInteger <$> choose (1, lim))
--   , Lambda <$> untlambda (succ lim) (size-1)
--   , App <$> untlambda lim (div size 2) <*> untlambda lim (div size 2)
--   ]


-- Arbitrary typed lambda expressions
instance Arbitrary Exp where
  arbitrary = sized (lambda 0)

lambda :: Int -> Int -> Gen Exp
lambda 0   0    = return $ Lambda (Var 1)
lambda 0   size = Lambda <$> lambda 1 (size-1)
lambda lim 0    = Var <$> (toInteger <$> (choose (1, lim)))
lambda lim size = oneof
  [ Var <$> (toInteger <$> choose (1, lim))
  , Lambda <$> lambda (succ lim) (size-1)
  , App <$> lambda lim (div size 2) <*> lambda lim (div size 2)
  , Pair <$> lambda lim (div size 2) <*> lambda lim (div size 2)
  , Pi1 <$> (Pair <$> lambda lim (div size 2) <*> lambda lim (div size 2))
  , Pi2 <$> (Pair <$> lambda lim (div size 2) <*> lambda lim (div size 2))
  , Inl <$> lambda lim (size-1)
  , Inr <$> lambda lim (size-1)
  , Caseof <$> (Inl <$> lambda lim (div size 3))
           <*> (Lambda <$> lambda (succ lim) (div size 3))
           <*> (Lambda <$> lambda (succ lim) (div size 3))
  , Caseof <$> (Inr <$> lambda lim (div size 3))
           <*> (Lambda <$> lambda (succ lim) (div size 3))
           <*> (Lambda <$> lambda (succ lim) (div size 3))
  , return Unit
  , Abort <$> lambda lim (size-1)
  , Absurd <$> lambda lim (size-1)
  ]

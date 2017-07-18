{-|
Module: Ski
Description: Ski expressions and bracket abstraction.
License: GPL-3

This module implements a representation of the SKI subset of the
calculus of combinators. It provides a lambda abstraction algorithm
writing lambda expressions as combinators.
-}

module Ski
  ( Ski (S, K, I, Comb)
  , skiabs
  )
where

import NamedLambda

-- | A SKI combinator expression
data Ski = S | K | I | Comb Ski Ski | Cte String
  deriving (Eq, Ord)

instance Show Ski where
  show = showski

-- | Shows a SKI expression
showski :: Ski -> String
showski S = "S"
showski K = "K"
showski I = "I"
showski (Comb x y) = "(" ++ showski x ++ showski y ++ ")"
showski (Cte _) = "?"

-- | SKI abstraction of a named lambda term. From a lambda expression
-- creates a SKI equivalent expression. The following algorithm is a
-- version of the algorithm 9.10 on the Hindley-Seldin book.
skiabs :: NamedLambda -> Ski
skiabs (LambdaVariable x) = Cte x
skiabs (LambdaApplication m n) = Comb (skiabs m) (skiabs n)
skiabs (LambdaAbstraction x m) = bracketabs x (skiabs m)

-- | Bracket abstraction of a SKI term, as defined in Hindley-Seldin
-- (2.18).
bracketabs :: String -> Ski -> Ski
bracketabs _ S = Comb K S
bracketabs _ K = Comb K K
bracketabs _ I = Comb K I
bracketabs x (Cte y) = if x == y then I else Comb K (Cte x)
bracketabs x (Comb u (Cte y))
  | freein x u && x == y = u
  | freein x u           = Comb K (Comb u (Cte y))
  | otherwise            = Comb (Comb S (bracketabs x u)) (bracketabs x (Cte y))
bracketabs x (Comb u v)
  | freein x (Comb u v) = Comb K (Comb u v)
  | otherwise           = Comb (Comb S (bracketabs x u)) (bracketabs x v)

-- | Checks if a given variable is used on a SKI expression.
freein :: String -> Ski -> Bool
freein _ S = True
freein _ K = True
freein _ I = True
freein x (Cte y)    = not (x == y)
freein x (Comb u v) = freein x u && freein x v


-- -- | Bracket abstraction of a lambda term. The following algorithm is
-- -- an adaptation to deBruijn indexes of the definition 2.18 and 9.10
-- -- of the Hindley-Seldin book.
-- skiabs :: Exp -> Ski

-- -- Error, the formula is not a closed one
-- skiabs (Var n) = undefined

-- -- The first case is the identity
-- skiabs (Lambda (Var 1)) = I

-- -- Only if the variable is free
-- skiabs (Lambda (App u (Var 1)))
--   | freein 1 u = skiabs u
--   | otherwise  = Comb (Comb S (skiabs u)) I

-- -- Combination
-- skiabs (Lambda m@(App u v))
--   | freein 1 m = Comb K (skiabs m)
--   | otherwise  = Comb (Comb S (skiabs u)) (skiabs v)

-- -- Error on pattern matching
-- skiabs (Lambda e) = undefined

-- skiabs (App u v) = Comb (skiabs u) (skiabs v)



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
         | Spair
         | Spi1
         | Spi2
         | Sinl
         | Sinr
         | Scase
         | Sunit
         | Sabort
  deriving (Eq, Ord)

instance Show Ski where
  show = showski

-- | Shows a SKI expression
showski :: Ski -> String
showski S = "S"
showski K = "K"
showski I = "I"
showski (Cte _) = "?"
showski (Comb x S) = showski x ++ showski S
showski (Comb x K) = showski x ++ showski K
showski (Comb x I) = showski x ++ showski I
showski (Comb x (Cte c)) = showski x ++ showski (Cte c)
showski (Comb x (Comb u v)) = showski x ++ "(" ++ showski (Comb u v) ++ ")"
showski (Comb x a) = showski x ++ showski a
showski (Spair) = "[PAIR]"
showski (Spi1) = "[FST]"
showski (Spi2) = "[SND]"
showski (Sinl) = "[INL]"
showski (Sinr) = "[INR]"
showski (Scase) = "[CASEOF]"
showski (Sunit) = "[UNIT]"
showski (Sabort) = "[ABORT]"

-- | SKI abstraction of a named lambda term. From a lambda expression
-- creates a SKI equivalent expression. The following algorithm is a
-- version of the algorithm 9.10 on the Hindley-Seldin book.
skiabs :: NamedLambda -> Ski
skiabs (LambdaVariable x) = Cte x
skiabs (LambdaApplication m n) = Comb (skiabs m) (skiabs n)
skiabs (LambdaAbstraction x m) = bracketabs x (skiabs m)
skiabs (TypedPair a b) = Comb (Comb Spair (skiabs a)) (skiabs b)
skiabs (TypedPi1 a) = Comb Spi1 (skiabs a)
skiabs (TypedPi2 a) = Comb Spi2 (skiabs a)
skiabs (TypedInl a) = Comb Sinl (skiabs a)
skiabs (TypedInr a) = Comb Sinr (skiabs a)
skiabs (TypedCase a b c) = Comb (Comb (Comb Scase (skiabs a)) (skiabs b)) (skiabs c)
skiabs (TypedUnit) = Sunit
skiabs (TypedAbort a) = Comb Sabort (skiabs a)

-- | Bracket abstraction of a SKI term, as defined in Hindley-Seldin
-- (2.18).
bracketabs :: String -> Ski -> Ski
bracketabs x (Cte y) = if x == y then I else Comb K (Cte y)
bracketabs x (Comb u (Cte y))
  | freein x u && x == y = u
  | freein x u           = Comb K (Comb u (Cte y))
  | otherwise            = Comb (Comb S (bracketabs x u)) (bracketabs x (Cte y))
bracketabs x (Comb u v)
  | freein x (Comb u v) = Comb K (Comb u v)
  | otherwise           = Comb (Comb S (bracketabs x u)) (bracketabs x v)
bracketabs _ a = Comb K a

-- | Checks if a given variable is used on a SKI expression.
freein :: String -> Ski -> Bool
freein x (Cte y)    = not (x == y)
freein x (Comb u v) = freein x u && freein x v
freein _ _ = True

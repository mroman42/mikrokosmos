{-|
Module: NamedLambda
Description: Lambda expressions with named variables
License: GPL-3

This package deals with lambda expressions containing named variables
instead of DeBruijn indexes. It contains parsing and printing fuctions.
-}

module NamedLambda
  ( NamedLambda (LambdaVariable, LambdaAbstraction, LambdaApplication)
  , lambdaexp
  )
where

import           Text.ParserCombinators.Parsec
import           Control.Applicative           ((<$>), (<*>))

-- Parsing of Lambda Expressions.
-- The user can input a lambda expression with named variables, of
-- the form of "\x.x" or "(\a.(\b.a b))". The interpreter will parse
-- it into an internal representation.

-- | A lambda expression with named variables.
data NamedLambda = LambdaVariable String                     -- ^ variable
                 | LambdaAbstraction String NamedLambda      -- ^ lambda abstraction
                 | LambdaApplication NamedLambda NamedLambda -- ^ function application

-- | Parses a lambda expression with named variables.
-- A lambda expression is a sequence of one or more autonomous
-- lambda expressions. They are parsed assuming left-associativity.
--
-- >>> parse lambdaexp "" "\\f.\\x.f x"
-- Right λf.λx.(f x)
--
-- Note that double backslashes are neccessary only when we quote strings;
-- it will work only with a simple backslash in the interpreter.
lambdaexp :: Parser NamedLambda
lambdaexp = foldl1 LambdaApplication <$> (spaces >> sepBy1 simpleexp spaces)

-- | Parses a simple lambda expression, without function applications
-- at the top level. It can be a lambda abstraction, a variable or another
-- potentially complex lambda expression enclosed in parentheses.
simpleexp :: Parser NamedLambda
simpleexp = choice [lambdaAbstractionParser, variableParser, parens lambdaexp]

-- | The returned parser parenthesizes the given parser
parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

-- | Parses a variable. Any name can form a lambda variable.
variableParser :: Parser NamedLambda
variableParser = LambdaVariable <$> nameParser

-- | Allowed variable names
nameParser :: Parser String
nameParser = many1 alphaNum

-- | Parses a lambda abstraction. The '\' is used as lambda. 
lambdaAbstractionParser :: Parser NamedLambda
lambdaAbstractionParser = LambdaAbstraction <$>
  (char lambdaChar >> nameParser) <*> (char '.' >> lambdaexp)

-- | Char used to represent lambda in user's input.
lambdaChar :: Char
lambdaChar = '\\'

-- | Shows a lambda expression with named variables.
-- Parentheses are ignored; they are written only around applications.
showNamedLambda :: NamedLambda -> String
showNamedLambda (LambdaVariable c)      = c
showNamedLambda (LambdaAbstraction c e) = "λ" ++ c ++ "." ++ showNamedLambda e ++ ""
showNamedLambda (LambdaApplication f g) = "(" ++ showNamedLambda f ++ " " ++ showNamedLambda g ++ ")"

instance Show NamedLambda where
  show = showNamedLambda



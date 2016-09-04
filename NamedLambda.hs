module NamedLambda where

import           Text.ParserCombinators.Parsec
import           Control.Applicative           ((<$>), (<*>))

-- Parsing of Lambda Expressions.
-- The user can input a lambda expression with named variables, of
-- the form of "\x.x" or "(\a.(\b.a b))". The interpreter will parse
-- it into an internal representation.

-- | A lambda expression with named variables.
data NamedLambda = LambdaVariable String               -- ^ variable
                 | LambdaAbstraction String NamedLambda -- ^ lambda abstraction 
                 | Lapp NamedLambda NamedLambda          -- ^ function application

-- | Parses a lambda expression with named variables.
-- A lambda expression is a sequence of one or more autonomous
-- lambda expressions. They are parsed assuming left-associativity.
lambdaexp :: Parser NamedLambda
lambdaexp = foldl1 Lapp <$> (spaces >> sepBy1 simpleexp spaces)

-- | Parses a simple lambda expression, without function applications
-- at the top level. It can be a lambda abstraction, a variable or another
-- potentially complex lambda expression enclosed in parentheses.
simpleexp :: Parser NamedLambda
simpleexp = choice [lambdaabs, variable, parens lambdaexp]

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

-- | Parses a variable. Any name can form a lambda variable.
variable :: Parser NamedLambda
variable = LambdaVariable <$> name

-- | Allowed variable names
name :: Parser String
name = many1 alphaNum

-- | Parses a lambda abstraction. The '\' is used as lambda. 
lambdaabs :: Parser NamedLambda
lambdaabs = LambdaAbstraction <$> (char lambdachar >> name) <*> (char '.' >> lambdaexp)

-- | Char used to represent lambda in user's input.
lambdachar :: Char
lambdachar = '\\'

-- | Shows a lambda expression with named variables.
-- Parentheses are ignored; they are written only around applications.
showlexp :: NamedLambda -> String
showlexp (LambdaVariable c)   = c
showlexp (LambdaAbstraction c e) = "λ" ++ c ++ "." ++ showlexp e ++ ""
showlexp (Lapp f g) = "(" ++ showlexp f ++ " " ++ showlexp g ++ ")"

instance Show NamedLambda where
  show = showlexp

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
  , toBruijn
  )
where

import           Text.ParserCombinators.Parsec
import           Control.Applicative           ((<$>), (<*>))
import qualified Data.Map.Strict               as Map
import           Lambda
import           MultiBimap
import           Data.Maybe

type Context  = MultiBimap Exp String

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




-- | Translates a named variable expression into a DeBruijn one.
-- Uses a dictionary of already binded numbers and variables.
tobruijn :: Map.Map String Integer -- ^ dictionary of the names of the variables used
         -> Context                -- ^ dictionary of the names already binded on the scope
         -> NamedLambda            -- ^ initial expression
         -> Exp
-- Every lambda abstraction is inserted in the variable dictionary,
-- and every number in the dictionary increases to reflect we are entering
-- into a deeper context.
tobruijn d context (LambdaAbstraction c e) = Lambda $ tobruijn newdict context e
  where newdict = Map.insert c 1 (Map.map succ d)
-- Translation of applications is trivial.
tobruijn d context (LambdaApplication f g) = App (tobruijn d context f) (tobruijn d context g)
-- Every variable is checked on the variable dictionary and in the current scope.
tobruijn d context (LambdaVariable c) =
  case Map.lookup c d of
    Just n  -> Var n
    Nothing -> fromMaybe (Var 0) (MultiBimap.lookupR c context)

-- | Transforms a lambda expression with named variables to a deBruijn index expression.
-- Uses only the dictionary of the variables in the current context. 
toBruijn :: Context     -- ^ Variable context
         -> NamedLambda  -- ^ Initial lambda expression with named variables
         -> Exp
toBruijn = tobruijn Map.empty

{-|
Module: NamedLambda
Description: Lambda expressions with named variables
License: GPL-3

This package deals with lambda expressions containing named variables
instead of DeBruijn indexes. It contains parsing and printing fuctions.
-}

module NamedLambda
  ( NamedLambda (LambdaVariable, LambdaAbstraction, LambdaApplication,
                 TypedPair, TypedPi1, TypedPi2,
                 TypedInl, TypedInr, TypedCase, TypedUnit, TypedAbort,
                 TypedAbsurd)
  , lambdaexp
  , toBruijn
  , nameExp
  , quicknameIndexes
  , variableNames
  )
where

import           Text.ParserCombinators.Parsec
import           Control.Applicative           ((<$>), (<*>))
import qualified Data.Map.Strict               as Map
import           Lambda
import           MultiBimap
import           Data.Maybe
import           Control.Monad

type Context  = MultiBimap Exp String

-- Parsing of Lambda Expressions.
-- The user can input a lambda expression with named variables, of
-- the form of "\x.x" or "(\a.(\b.a b))". The interpreter will parse
-- it into an internal representation.

-- | A lambda expression with named variables.
data NamedLambda = LambdaVariable String                         -- ^ variable
                 | LambdaAbstraction String NamedLambda          -- ^ lambda abstraction
                 | LambdaApplication NamedLambda NamedLambda     -- ^ function application
                 | TypedPair NamedLambda NamedLambda             -- ^ pair of expressions
                 | TypedPi1  NamedLambda                         -- ^ first projection
                 | TypedPi2  NamedLambda                         -- ^ second projection
                 | TypedInl  NamedLambda                         -- ^ left injection
                 | TypedInr  NamedLambda                         -- ^ right injection
                 | TypedCase NamedLambda NamedLambda NamedLambda -- ^ case of expressions
                 | TypedUnit                                     -- ^ unit
                 | TypedAbort NamedLambda                        -- ^ abort
                 | TypedAbsurd NamedLambda                       -- ^ absurd
                 deriving (Eq)

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
simpleexp = choice
  [ try pairParser
  , try pi1Parser
  , try pi2Parser
  , try inlParser
  , try inrParser
  , try caseParser
  , try unitParser
  , try abortParser
  , try absurdParser
  , try lambdaAbstractionParser
  , try variableParser
  , try (parens lambdaexp)
  ]

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
  (lambdaChar >> nameParser) <*> (char '.' >> lambdaexp)

-- | Char used to represent lambda in user's input.
lambdaChar :: Parser Char
lambdaChar = choice [try $ char '\\', try $ char 'λ']

pairParser :: Parser NamedLambda
pairParser = parens (TypedPair <$> lambdaexp <*> (char ',' >> lambdaexp))

pi1Parser, pi2Parser :: Parser NamedLambda
pi1Parser = TypedPi1 <$> (string "FST " >> lambdaexp)
pi2Parser = TypedPi2 <$> (string "SND " >> lambdaexp)

inlParser, inrParser :: Parser NamedLambda
inlParser = TypedInl <$> (string "INL " >> lambdaexp)
inrParser = TypedInr <$> (string "INR " >> lambdaexp)

caseParser :: Parser NamedLambda
caseParser = TypedCase <$> (string "CASE " >> simpleexp) <*> (string " OF " >> simpleexp) <*> (string ";" >> simpleexp)

unitParser :: Parser NamedLambda
unitParser = string "UNIT" >> return TypedUnit

abortParser :: Parser NamedLambda
abortParser = TypedAbort <$> (string "ABORT " >> lambdaexp)

absurdParser :: Parser NamedLambda
absurdParser = TypedAbsurd <$> (string "ABSURD " >> lambdaexp)

-- | Shows a lambda expression with named variables.
-- Parentheses are ignored; they are written only around applications.
showNamedLambda :: NamedLambda -> String
showNamedLambda (LambdaVariable c)      = c
showNamedLambda (LambdaAbstraction c e) = "λ" ++ c ++ "." ++ showNamedLambda e ++ ""
showNamedLambda (LambdaApplication f g) = "(" ++ showNamedLambda f ++ " " ++ showNamedLambda g ++ ")"
showNamedLambda (TypedPair a b)         = "(" ++ showNamedLambda a ++ "," ++ showNamedLambda b ++ ")"
showNamedLambda (TypedPi1 a)            = "π₁ " ++ showNamedLambda a
showNamedLambda (TypedPi2 a)            = "π₂ " ++ showNamedLambda a
showNamedLambda (TypedInl a)            = "(" ++ "INL " ++ showNamedLambda a ++ ")"
showNamedLambda (TypedInr a)            = "(" ++ "INR " ++ showNamedLambda a ++ ")"
showNamedLambda (TypedCase a b c)       = "(" ++ "CASE " ++ showNamedLambda a ++ " OF " ++ showNamedLambda b ++ "; " ++ showNamedLambda c ++ ")"
showNamedLambda TypedUnit             = "UNIT"
showNamedLambda (TypedAbort a)          = "(" ++ "ABORT " ++ showNamedLambda a ++ ")"
showNamedLambda (TypedAbsurd a)          = "(" ++ "ABSURD " ++ showNamedLambda a ++ ")"

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
tobruijn d context (TypedPair a b) = Pair (tobruijn d context a) (tobruijn d context b)
tobruijn d context (TypedPi1 a) = Pi1 (tobruijn d context a)
tobruijn d context (TypedPi2 a) = Pi2 (tobruijn d context a)
tobruijn d context (TypedInl a) = Inl (tobruijn d context a)
tobruijn d context (TypedInr a) = Inr (tobruijn d context a)
tobruijn d context (TypedCase a b c) = Caseof (tobruijn d context a) (tobruijn d context b) (tobruijn d context c)
tobruijn _ _       TypedUnit = Unit
tobruijn d context (TypedAbort a) = Abort (tobruijn d context a)
tobruijn d context (TypedAbsurd a) = Absurd (tobruijn d context a)

-- | Transforms a lambda expression with named variables to a deBruijn index expression.
-- Uses only the dictionary of the variables in the current context. 
toBruijn :: Context     -- ^ Variable context
         -> NamedLambda -- ^ Initial lambda expression with named variables
         -> Exp
toBruijn = tobruijn Map.empty



-- | Translates a deBruijn expression into a lambda expression
-- with named variables, given a list of used and unused variable names.
nameIndexes :: [String] -> [String] -> Exp -> NamedLambda
nameIndexes _    _   (Var 0)        = LambdaVariable "undefined"
nameIndexes used _   (Var n)        = LambdaVariable (used !! pred (fromInteger n))
nameIndexes used new (Lambda e)     = LambdaAbstraction (head new) (nameIndexes (head new:used) (tail new) e)
nameIndexes used new (App f g)      = LambdaApplication (nameIndexes used new f) (nameIndexes used new g)
nameIndexes used new (Pair a b)     = TypedPair (nameIndexes used new a) (nameIndexes used new b)
nameIndexes used new (Pi1 a)        = TypedPi1 (nameIndexes used new a)
nameIndexes used new (Pi2 a)        = TypedPi2 (nameIndexes used new a)
nameIndexes used new (Inl a)        = TypedInl (nameIndexes used new a)
nameIndexes used new (Inr a)        = TypedInr (nameIndexes used new a)
nameIndexes used new (Caseof a b c) = TypedCase (nameIndexes used new a) (nameIndexes used new b) (nameIndexes used new c)
nameIndexes _    _   Unit           = TypedUnit
nameIndexes used new (Abort a)      = TypedAbort (nameIndexes used new a)
nameIndexes used new (Absurd a)     = TypedAbsurd (nameIndexes used new a)


quicknameIndexes :: Int -> [String] -> Exp -> NamedLambda
quicknameIndexes _ _  (Var 0)          = LambdaVariable "undefined"
quicknameIndexes n vars   (Var m)      = LambdaVariable (vars !! (n - fromInteger m))
quicknameIndexes n vars (Lambda e)     = LambdaAbstraction (vars !! n) (quicknameIndexes (succ n) vars e)
quicknameIndexes n vars (App f g)      = LambdaApplication (quicknameIndexes n vars f) (quicknameIndexes n vars g)
quicknameIndexes n vars (Pair a b)     = TypedPair (quicknameIndexes n vars a) (quicknameIndexes n vars b)
quicknameIndexes n vars (Pi1 a)        = TypedPi1 (quicknameIndexes n vars a)
quicknameIndexes n vars (Pi2 a)        = TypedPi2 (quicknameIndexes n vars a)
quicknameIndexes n vars (Inl a)        = TypedInl (quicknameIndexes n vars a)
quicknameIndexes n vars (Inr a)        = TypedInr (quicknameIndexes n vars a)
quicknameIndexes n vars (Caseof a b c) = TypedCase (quicknameIndexes n vars a) (quicknameIndexes n vars b) (quicknameIndexes n vars c)
quicknameIndexes _ _   Unit            = TypedUnit
quicknameIndexes n vars (Abort a)      = TypedAbort (quicknameIndexes n vars a)
quicknameIndexes n vars (Absurd a)     = TypedAbsurd (quicknameIndexes n vars a)


-- | Gives names to every variable in a deBruijn expression using
-- alphabetic order.
nameExp :: Exp -> NamedLambda
nameExp = nameIndexes [] variableNames

-- | A list of all possible variable names in lexicographical order.
variableNames :: [String]
variableNames = concatMap (`replicateM` ['a'..'z']) [1..]

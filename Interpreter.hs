{-|
Module: Interpreter
Description: Internal logic of the interpreter
License: GPL-3

This module contains auxiliary logic, types and representations of
the internal state of the interpreter.
-}
module Interpreter
  ( Context
  , emptyContext
  , InterpreterOptions (InterpreterOptions)
  , defaultOptions
  , changeVerbose
  , changeColor
  , getVerbose
  , getColor
  , InterpreterAction (..)
  , interpreteractionParser
  , Action (..)
  , actionParser
  )
where

import           Control.Applicative           ((<$>), (<*>))
import           Text.ParserCombinators.Parsec
import           Data.Char
import           MultiBimap
import           NamedLambda
import           Lambda

-- | A context is an application between expressions and the names
-- they may have.
type Context  = MultiBimap Exp String

-- | Empty context without any bindings
emptyContext :: Context
emptyContext = MultiBimap.empty


-- Interpreter options
-- | Configuration options for the interpreter. They can be changed dinamically.
data InterpreterOptions = InterpreterOptions
  { verbose :: Bool -- ^ true to produce verbose output
  , color :: Bool   -- ^ true to color the output
  }

-- | Default configuration options for the interpreter.
defaultOptions :: InterpreterOptions
defaultOptions = InterpreterOptions
  { verbose = False
  , color   = True
  }

-- | Gets the verbose configuration
getVerbose :: InterpreterOptions -> Bool
getVerbose = verbose

-- | Gets the color configuration
getColor :: InterpreterOptions -> Bool
getColor = color

-- | Sets the verbose configuration on/off.
changeVerbose :: InterpreterOptions -> InterpreterOptions
changeVerbose options = options {verbose = not $ verbose options}

-- | Sets the color configuration on/off
changeColor :: InterpreterOptions -> InterpreterOptions
changeColor options = options {color = not $ color options}





-- | Interpreter action. It can be a language action (binding and evaluation)
-- or an interpreter specific one, such as "quit". 
data InterpreterAction = Interpret Action -- ^ Language action
                       | EmptyLine        -- ^ Empty line, it will be ignored
                       | Error            -- ^ Error on the interpreter
                       | Quit             -- ^ Close the interpreter
                       | Load String      -- ^ Load the given file
                       | SetVerbose       -- ^ Changes verbosity
                       | SetColors        -- ^ Changes colors
                       | Help             -- ^ Shows help

-- | Language action. The language has a number of possible valid statements;
-- all on the following possible forms.
data Action = Bind (String, NamedLambda)     -- ^ bind a name to an expression
            | EvalBind (String, NamedLambda) -- ^ bind a name to an expression and simplify it
            | Execute NamedLambda            -- ^ execute an expression
            | Comment                        -- ^ comment



-- Parsing of interpreter command line commands.
-- | Parses an interpreter action.
interpreteractionParser :: Parser InterpreterAction
interpreteractionParser = choice
  [ try interpretParser
  , try quitParser
  , try loadParser
  , try verboseParser
  , try helpParser
  ]

-- | Parses a language action as an interpreter action.
interpretParser :: Parser InterpreterAction
interpretParser = Interpret <$> actionParser

-- | Parses a language action.
actionParser :: Parser Action
actionParser = choice
  [ try bindParser
  , try evalbindParser
  , try executeParser
  , try commentParser
  ]

-- | Parses a binding between a variable an its representation.
bindParser :: Parser Action
bindParser = fmap Bind $ (,) <$> many1 alphaNum <*> (spaces >> string "!=" >> spaces >> lambdaexp)

-- | Parses a binding and evaluation expression between a variable an its representation
evalbindParser :: Parser Action
evalbindParser = fmap EvalBind $ (,) <$> many1 alphaNum <*> (spaces >> string "=" >> spaces >> lambdaexp)

-- | Parses an expression in order to execute it.
executeParser :: Parser Action
executeParser = Execute <$> lambdaexp

-- | Parses comments.
commentParser :: Parser Action
commentParser = string "#" >> many anyChar >> return Comment

-- | Parses a "quit" command.
quitParser :: Parser InterpreterAction
quitParser = string ":quit" >> return Quit

-- | Parses a "help" command.
helpParser :: Parser InterpreterAction
helpParser = string ":help" >> return Help

-- | Parses a change in verbosity.
verboseParser :: Parser InterpreterAction
verboseParser = string ":verbose" >> return SetVerbose

-- | Parses a "load-file" command.
loadParser :: Parser InterpreterAction
loadParser = Load <$> (string ":load" >> between spaces spaces (many1 (satisfy (not . isSpace))))

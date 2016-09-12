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
  , act
  , multipleAct
  , Action (..)
  , actionParser
  )
where

import           Control.Applicative           ((<$>), (<*>))
import           Control.Monad.State.Lazy      
import           Text.ParserCombinators.Parsec hiding (State)
import           Data.Char
import           Data.List
import           MultiBimap
import           NamedLambda
import           Format
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


-- | Executes a language action. Given a context and an action, returns
-- the new context after the action and a text output.
act :: Action -> State Context [String]
act Comment = return [""]
act (Bind (s,le)) =
  do modify (\ctx -> MultiBimap.insert (toBruijn ctx le) s ctx)
     return [""]
act (EvalBind (s,le)) =
  do modify (\ctx -> MultiBimap.insert (simplifyAll $ toBruijn ctx le) s ctx)
     return [""]
act (Execute le) =
  do context <- get
     return [unlines $
             [ show le ] ++
             [ unlines $ map showReduction $ simplifySteps $ toBruijn context le ] ++
             [ showCompleteExp context $ simplifyAll $ toBruijn context le ]
            ]


-- TODO: Use Text instead of String for efficiency
-- | Executes multiple actions. Given a context and a set of actions, returns
-- the new context after the sequence of actions and a text output.
multipleAct :: [Action] -> State Context [String]
multipleAct actions = concat <$> mapM act actions



-- | Shows an expression and the name that is bound to the expression
-- in the current context
showCompleteExp :: Context -> Exp -> String
showCompleteExp context expr = case getExpressionName context expr of
  Nothing      -> show (nameExp expr)
  Just expName -> show (nameExp expr) ++ formatName ++ " ⇒ " ++ expName ++ end


-- | Given an expression, returns its name if it is bounded to any.
getExpressionName :: Context -> Exp -> Maybe String
getExpressionName context expr = case MultiBimap.lookup expr context of
  [] -> Nothing
  xs -> Just $ intercalate ", " xs




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

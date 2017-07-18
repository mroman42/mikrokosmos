{-|
Module: Interpreter
Description: Internal logic of the interpreter
License: GPL-3

This module contains auxiliary logic, types and representations of
the internal state of the interpreter.
-}
module Interpreter
  (  
  -- * Interpreter actions
    InterpreterAction (..)
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
import           Format
import           Environment
import           NamedLambda
import           Lambda
import           Ski


-- | Interpreter action. It can be a language action (binding and evaluation)
-- or an interpreter specific one, such as "quit".
data InterpreterAction = Interpret Action -- ^ Language action
                       | EmptyLine        -- ^ Empty line, it will be ignored
                       | Error            -- ^ Error on the interpreter
                       | Quit             -- ^ Close the interpreter
                       | Load String      -- ^ Load the given file
                       | SetVerbose Bool  -- ^ Changes verbosity
                       | SetColor Bool    -- ^ Changes colors
                       | Help             -- ^ Shows help

-- | Language action. The language has a number of possible valid statements;
-- all on the following possible forms.
data Action = Bind (String, NamedLambda)     -- ^ bind a name to an expression
            | EvalBind (String, NamedLambda) -- ^ bind a name to an expression and simplify it
            | Execute NamedLambda            -- ^ execute an expression
            | Comment                        -- ^ comment


-- | Executes a language action. Given a context and an action, returns
-- the new context after the action and a text output.
act :: Action -> State Environment [String]
act Comment = return [""]
act (Bind (s,le)) =
  do modify (\env -> addBind env s (toBruijn (context env) le))
     return [""]
act (EvalBind (s,le)) =
  do modify (\env -> addBind env s (simplifyAll $ toBruijn (context env) le))
     return [""]
act (Execute le) =
  do env <- get
     return [unlines $
              [ show le ] ++
              [ unlines $ map showReduction $ simplifySteps $ toBruijn (context env) le ] ++
              [ showCompleteExp env $ simplifyAll $ toBruijn (context env) le ] 
            ]


-- | Executes multiple actions. Given a context and a set of actions, returns
-- the new context after the sequence of actions and a text output.
multipleAct :: [Action] -> State Environment [String]
multipleAct actions = concat <$> mapM act actions


-- | Shows an expression and the name that is bound to the expression
-- in the current context
showCompleteExp :: Environment -> Exp -> String
showCompleteExp environment expr = let
      lambdaname = show $ nameExp expr
      skiname = formatSubs2 ++ " ⇒ " ++ (show $ skiabs $ nameExp expr) ++ end
  in
  case getExpressionName environment expr of
    Nothing      -> lambdaname ++ skiname
    Just expName -> lambdaname ++ skiname ++ formatName ++ " ⇒ " ++ expName ++ end 
    





-- Parsing of interpreter command line commands.
-- | Parses an interpreter action.
interpreteractionParser :: Parser InterpreterAction
interpreteractionParser = choice
  [ try interpretParser
  , try quitParser
  , try loadParser
  , try verboseParser
  , try colorParser
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
verboseParser = choice
  [ try verboseonParser
  , try verboseoffParser
  ]
  where
    verboseonParser  = string ":verbose on" >> return (SetVerbose True)
    verboseoffParser = string ":verbose off" >> return (SetVerbose False)

-- | Parses a change in color.
colorParser :: Parser InterpreterAction
colorParser = choice
  [ try coloronParser
  , try coloroffParser
  ]
  where
    coloronParser  = string ":color on" >> return (SetColor True)
    coloroffParser = string ":color off" >> return (SetColor False)


-- | Parses a "load-file" command.
loadParser :: Parser InterpreterAction
loadParser = Load <$> (string ":load" >> between spaces spaces (many1 (satisfy (not . isSpace))))

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
import           Types


-- | Interpreter action. It can be a language action (binding and evaluation)
-- or an interpreter specific one, such as "quit".
data InterpreterAction = Interpret Action -- ^ Language action
                       | EmptyLine        -- ^ Empty line, it will be ignored
                       | Error            -- ^ Error on the interpreter
                       | Quit             -- ^ Close the interpreter
                       | Restart          -- ^ Restarts the environment
                       | Load String      -- ^ Load the given file
                       | SetVerbose Bool  -- ^ Changes verbosity
                       | SetColor Bool    -- ^ Changes colors
                       | SetSki Bool      -- ^ Changes ski output
                       | SetTypes Bool    -- ^ Changes type configuration
                       | SetTopo Bool
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
act (Execute le) = executeExpression le

-- | Executes a lambda expression. Given the context, returns the new
-- context after the evaluation.
executeExpression :: NamedLambda -> State Environment [String]
executeExpression le = do
     env <- get
     let typed = getTypes env
     let bruijn = toBruijn (context env) le
     let illtyped = typed && typeinference bruijn == Nothing
     let notypes = not typed && usestypecons bruijn
     let verbose = getVerbose env
     let completeexp = showCompleteExp env $ simplifyAll $ bruijn
     let isopen = isOpenExp bruijn
     
     return $
       if isopen then [errorUndefinedText ++ "\n"] else
       if illtyped then [errorNonTypeableText ++ "\n"] else
       if notypes then [errorTypeConstructors ++ "\n"] else
       if not verbose then [completeexp ++ "\n"] else
         [unlines $
           [show le] ++
           [unlines $ map showReduction $ simplifySteps $ bruijn] ++
           [completeexp]
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
      skiname = if getSki environment
                 then formatSubs2 ++ " ⇒ " ++ (show $ skiabs $ nameExp expr) ++ end
                 else ""
      inferredtype = typeinference expr
      typename = if getTypes environment
                  then formatType ++ " :: " ++ (case inferredtype of
                                                   Just s -> if getTopo environment
                                                             then show (Top s)
                                                             else show s
                                                   Nothing -> "Type error!") ++ end
                  else ""
      expName = case getExpressionName environment expr of
                  Nothing -> ""
                  Just exname -> formatName ++ " ⇒ " ++ exname ++ end
  in
      lambdaname ++ skiname ++ expName ++ typename ++ end
    


-- Parsing of interpreter command line commands.
-- | Parses an interpreter action.
interpreteractionParser :: Parser InterpreterAction
interpreteractionParser = choice
  [ try interpretParser
  , try quitParser
  , try restartParser
  , try loadParser
  , try verboseParser
  , try colorParser
  , try skiOutputParser
  , try typesParser
  , try topoParser
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
quitParser, restartParser, helpParser :: Parser InterpreterAction
quitParser    = string ":quit" >> return Quit
restartParser = string ":restart" >> return Restart
helpParser    = string ":help" >> return Help

-- | Parses a change in a setting
settingParser :: (Bool -> InterpreterAction) -> String -> Parser InterpreterAction
settingParser setSetting settingname = choice
  [ try settingonParser
  , try settingoffParser
  ]
  where
    settingonParser  = string (settingname ++ " on")  >> return (setSetting True)
    settingoffParser = string (settingname ++ " off") >> return (setSetting False)

-- | Multiple setting parsers
verboseParser, colorParser, skiOutputParser, typesParser, topoParser :: Parser InterpreterAction
verboseParser   = settingParser SetVerbose ":verbose"
colorParser     = settingParser SetColor   ":color"
skiOutputParser = settingParser SetSki     ":ski"
typesParser     = settingParser SetTypes   ":types"
topoParser      = settingParser SetTopo    ":Hausdorff"


-- | Parses a "load-file" command.
loadParser :: Parser InterpreterAction
loadParser = Load <$> (string ":load" >> between spaces spaces (many1 (satisfy (not . isSpace))))

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
import           Data.Maybe
import           Format
import           Environment
import           NamedLambda
import           Lambda
import           Ski
import           Types
import           Gentzen


-- | Interpreter action. It can be a language action (binding and evaluation)
-- or an interpreter specific one, such as "quit".
data InterpreterAction = Interpret Action -- ^ Language action
                       | Quit             -- ^ Close the interpreter
                       | Load String      -- ^ Load the given file

-- | Language action. The language has a number of possible valid statements;
-- all on the following possible forms.
data Action = Bind (String, NamedLambda)     -- ^ bind a name to an expression
            | EvalBind (String, NamedLambda) -- ^ bind a name to an expression and simplify it
            | Execute NamedLambda            -- ^ execute an expression
            | Diagram NamedLambda            -- ^ show the diagram of a lambda expression
            | SimpDiagram NamedLambda        -- ^ show the diagram of the simplified expression
            | Comment                        -- ^ comment
            | EmptyLine                      -- ^ empty line, it will be ignored
            | Error                          -- ^ error on the interpreter
            | Restart                        -- ^ restarts the environment
            | Help                           -- ^ shows help
            | SetVerbose Bool                -- ^ changes verbosity
            | SetColor Bool                  -- ^ changes colors
            | SetSki Bool                    -- ^ changes ski output
            | SetTypes Bool                  -- ^ changes type configuration
            | SetTopo Bool

              
-- | Executes a language action. Given a context and an action, returns
-- the new context after the action and a text output.
act :: Action -> State Environment [String]
act Comment = return [""]
act (Bind (s,le)) = do
  modify (\env -> addBind env s (toBruijn (context env) le))
  return [""]
act (EvalBind (s,le)) = do
  modify (\env -> addBind env s (simplifyAll $ toBruijn (context env) le))
  return [""]
act (Execute le) = executeExpression le
act (Diagram le) = drawDiagram False le
act (SimpDiagram le) = drawDiagram True le
act EmptyLine = return [""]
act Error = return [errorUnknownCommand ++ "\n"]
act Restart = put defaultEnv >> return [restartText ++ "\n"]
act Help = return [helpText ++ "\n"]
act (SetVerbose setting) = setOption setting changeVerbose "verbose: "
act (SetColor setting) = setOption setting changeColor "color mode: "
act (SetSki setting) = setOption setting changeSkioutput "ski mode: "
act (SetTypes setting) = setOption setting changeTypes "types: "
act (SetTopo setting) = setOption setting changeTopo "topology: "


-- | Sets the given option on/off.
setOption :: Bool ->
             (Environment -> Bool -> Environment) ->
             String ->
             State Environment [String]
setOption setting change message = do
  modify (`change` setting)
  env <- get
  return [messageenv env ++ "\n"]
    where messageenv env =
            (if getColor env then formatFormula else "") ++ message ++
            (if setting then "on" else "off") ++ end

drawDiagram :: Bool -> NamedLambda -> State Environment [String]
drawDiagram wantsimplification le = do
  env <- get
  let bruijn = toBruijn (context env) le
  let isopen = isOpenExp bruijn
  let simpbruijn = simplifyAll bruijn
  let maybediagram = gentzendiagram (if wantsimplification then simpbruijn else bruijn)

  return $
    if isopen then [errorUndefinedText ++ "\n"] else
      case maybediagram of
        Nothing -> [errorNonTypeableText ++ "\n"]
        Just diagram -> map (++ "\n") . lines $ showProofTree diagram
  
-- | Executes a lambda expression. Given the context, returns the new
-- context after the evaluation.
executeExpression :: NamedLambda -> State Environment [String]
executeExpression le = do
     env <- get
     let typed = getTypes env
     let bruijn = toBruijn (context env) le
     let illtyped = typed && isNothing (typeinference bruijn)
     let notypes = not typed && usestypecons bruijn
     let verbose = getVerbose env
     let completeexp = showCompleteExp env $ simplifyAll bruijn
     let isopen = isOpenExp bruijn
     let coloring = getColor env
     let decolorif = if coloring then id else decolor
     
     return $
       if isopen then [errorUndefinedText ++ "\n"] else
       if illtyped then [errorNonTypeableText ++ "\n"] else
       if notypes then [errorTypeConstructors ++ "\n"] else
       if not verbose then [completeexp ++ "\n"] else
         [unlines $
           [show le] ++
           [unlines $ map (decolorif . showReduction) $ simplifySteps bruijn] ++
           [completeexp]
         ]
  

-- | Executes multiple actions. Given a context and a set of actions, returns
-- the new context after the sequence of actions and a text output.
multipleAct :: [Action] -> State Environment [String]
multipleAct actions = concat <$> mapM act actions


-- | Shows an expression and the name that is bound to the expression
-- in the current context
showCompleteExp :: Environment -> Exp -> String
showCompleteExp environment expr = 
  lambdaname ++ skiname ++ expName ++ typename ++ end
  where
    coloring = getColor environment
    ifvoid b x = if b then x else ""
    typesset = getTypes environment
    skiset = getSki environment
    lambdaname = show $ nameExp expr
    skiname = ifvoid skiset $ formatSubs2 ++ " ⇒ " ++ show (skiabs $ nameExp expr) ++ end
    inferredtype = case typeinference expr of
                     Just s -> if getTopo environment then show (Top s) else show s
                     Nothing -> "Type error!"
    typename = ifvoid typesset $ ifvoid coloring formatType ++ " :: " ++ inferredtype ++ end
    expName = case getExpressionName environment expr of
                Just exname -> ifvoid coloring formatName ++ " ⇒ " ++ exname ++ end
                Nothing -> ""


-- Parsing of interpreter command line commands.
-- | Parses an interpreter action.
interpreteractionParser :: Parser InterpreterAction
interpreteractionParser = choice
  [ try interpretParser
  , try quitParser
  , try loadParser
  ]

-- | Parses a language action as an interpreter action.
interpretParser :: Parser InterpreterAction
interpretParser = Interpret <$> actionParser

-- | Parses a language action.
actionParser :: Parser Action
actionParser = choice $ map try
  [ simpdiagramParser
  , diagramParser
  , bindParser
  , evalbindParser
  , executeParser
  , commentParser
  , helpParser
  , restartParser
  , verboseParser
  , colorParser
  , skiOutputParser
  , typesParser
  , topoParser
  ]


-- | Parses a diagram request
diagramParser :: Parser Action
diagramParser = Diagram <$> (string "@ " >> lambdaexp)

-- | Parses a simplfication and diagram request
simpdiagramParser :: Parser Action
simpdiagramParser = SimpDiagram <$> (string "@@ " >> lambdaexp)

-- | Parses a binding between a variable an its representation.
bindParser :: Parser Action
bindParser = fmap Bind $ (,) <$>
  many1 alphaNum <*>
  (spaces >> choice [try (string "!="), try (string ":=")] >> spaces >> lambdaexp)

-- | Parses a binding and evaluation expression between a variable an its representation
evalbindParser :: Parser Action
evalbindParser = fmap EvalBind $ (,) <$> many1 alphaNum <*> (spaces >> string "=" >> spaces >> lambdaexp)

-- | Parses an expression in order to execute it.
executeParser :: Parser Action
executeParser = Execute <$> lambdaexp

-- | Parses comments.
commentParser :: Parser Action
commentParser = string "#" >> many anyChar >> return Comment

quitParser :: Parser InterpreterAction
quitParser    = string ":quit" >> return Quit
restartParser, helpParser :: Parser Action
restartParser = string ":restart" >> return Restart
helpParser    = string ":help" >> return Help

-- | Parses a change in a setting
settingParser :: (Bool -> Action) -> String -> Parser Action
settingParser setSetting settingname = choice
  [ try settingonParser
  , try settingoffParser
  ]
  where
    settingonParser  = string (settingname ++ " on")  >> return (setSetting True)
    settingoffParser = string (settingname ++ " off") >> return (setSetting False)

-- | Multiple setting parsers
verboseParser, colorParser, skiOutputParser, typesParser, topoParser :: Parser Action
verboseParser   = settingParser SetVerbose ":verbose"
colorParser     = settingParser SetColor   ":color"
skiOutputParser = settingParser SetSki     ":ski"
typesParser     = settingParser SetTypes   ":types"
topoParser      = settingParser SetTopo    ":topology"


-- | Parses a "load-file" command.
loadParser :: Parser InterpreterAction
loadParser = Load <$> (string ":load" >> between spaces spaces (many1 (satisfy (not . isSpace))))

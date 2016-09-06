module Main where

import           Control.Applicative           ((<$>), (<*>))
import           Control.Monad.Trans
import           Data.Char
import           Data.List
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           System.Environment
import           System.Console.Haskeline
import           Text.ParserCombinators.Parsec
import           Format
import           MultiBimap
import           NamedLambda
import           Lambda

type Filename = String
type Context = MultiBimap.MultiBimap Exp String




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




-- Lambda interpreter
-- The logic of the interpreter is written here. It allows to execute normal
-- actions (bindings and evaluation), and interpreter specific actions, as
-- "quit" or "load".

-- | Runs the interpreter with default settings and an empty context.
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runInputT defaultSettings (outputStrLn initText
                                  >> interpreterLoop defaultOptions emptyContext)
    [filename] -> executeFile filename
    _ -> putStrLn "Wrong number of arguments"


-- | Executes the commands inside a file.
executeFile :: Filename -> IO ()
executeFile filename = do
  maybeloadfile <- loadFile filename
  case maybeloadfile of
    Nothing    -> putStrLn "Error loading file"
    Just actions -> case multipleAct emptyContext actions of
                      (_, outputs) -> mapM_ (putStr . format) outputs
                      where
                        format :: String -> String
                        format "" = ""
                        format s = (++"\n") . last . lines $ s
  

-- | Empty context without any bindings
emptyContext :: Context
emptyContext = MultiBimap.empty


-- | Configuration options for the interpreter. They can be changed dinamically.
data InterpreterOptions = InterpreterOptions { verbose :: Bool -- ^ true if produces verbose output
                                             , color :: Bool   -- ^ true if colors the output
                                             }

-- | Default config options
defaultOptions :: InterpreterOptions
defaultOptions = InterpreterOptions { verbose = False
                                    , color   = True
                                    }

-- TODO: Help
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
            -- Derives Show for debugging purposes only. 
            deriving (Show)

-- | Executes a language action. Given a context and an action, returns
-- the new context after the action and a text output.
act :: Context -> Action -> (Context, String)
act context Comment           = (context,"")
act context (Bind (s,le))     = (MultiBimap.insert (toBruijn context le) s context, "")
act context (EvalBind (s,le)) = (MultiBimap.insert (simplifyall $ toBruijn context le) s context, "")
act context (Execute le)  = (context,
                             unlines $
                              [ show le ] ++
                              [ unlines $ map show $ stepsSimplify $ toBruijn context le ] ++
                              [ showCompleteExp context $ simplifyall $ toBruijn context le ]
                            )

showCompleteExp :: Context -> Exp -> String
showCompleteExp context expr = case getExpressionName context expr of
  Nothing      -> show expr
  Just expName -> show expr ++ formatName ++ " ⇒ " ++ expName ++ end

getExpressionName :: Context -> Exp -> Maybe String
getExpressionName context expr = case MultiBimap.lookup expr context of
  [] -> Nothing
  xs -> Just $ intercalate ", " xs

-- TODO: Writer monad
-- TODO: Use Text instead of String for efficiency
-- TODO: Lists of string are inefficient
-- | Executes multiple actions. Given a context and a set of actions, returns
-- the new context after the sequence of actions and a text output.
multipleAct :: Context -> [Action] -> (Context, [String])
multipleAct context = foldl (\(ccontext,text) action ->
                                (fst $ act ccontext action, text ++ [snd (act ccontext action)]))
                      (context,[])


-- | Prompt line
prompt :: String
prompt = formatPrompt ++ "mikroλ> " ++ end

-- | Help line
helpStr :: String
helpStr = unlines [
  formatFormula ++
  "Commands available from the prompt:",
  "\t<expression>\t evaluates the expression",
  "\t:quit       \t quits the interpreter",
  "\t:load <file>\t loads the given .mkr library or script",
  "\t:verbose    \t sets verbose mode on/off",
--  "\t:color      \t sets terminal colors on/off",
  "\t:help       \t shows this help"
  ++ end
  ]

-- TODO: State Monad
-- | Interpreter awaiting for an instruction.
interpreterLoop :: InterpreterOptions -> Context -> InputT IO ()
interpreterLoop options context = do
  minput <- getInputLine prompt
  let interpreteraction =
        case minput of
          Nothing -> Quit
          Just "" -> EmptyLine
          Just input -> case parse interpreteractionParser "" input of
            Left _  -> Error
            Right a -> a
  case interpreteraction of
    EmptyLine -> interpreterLoop options context
    Quit -> return ()
    Error -> outputStrLn "Error"
    SetVerbose -> interpreterLoop (options {verbose = not $ verbose options}) context
    SetColors  -> interpreterLoop (options {color   = not $ color   options}) context
    Help -> outputStr helpStr >> interpreterLoop options context
    Load filename -> do
      maybeloadfile <- lift $ loadFile filename
      case maybeloadfile of
        Nothing    -> outputStrLn "Error loading file"
        Just actions -> case multipleAct context actions of
                          (ccontext, outputs) -> do
                            outputStr formatFormula
                            outputActions options outputs
                            outputStr end
                            interpreterLoop options ccontext
    Interpret action -> case act context action of
                          (ccontext, output) -> do
                            outputStr formatFormula
                            outputActions options [output]
                            outputStr end
                            interpreterLoop options ccontext

-- | Outputs results from actions. Given a list of options and outputs,
-- formats and prints them in console.
outputActions :: InterpreterOptions -> [String] -> InputT IO ()
outputActions options = mapM_ (outputStr . format)
  where
    format :: String -> String
    format "" = ""
    format s
      | not (verbose options) = (++"\n") . last . lines $ s
      | otherwise             = s

-- | Loads the given filename and returns the complete list of actions.
-- Returns Nothing if there is an error reading or parsing the file.
loadFile :: String -> IO (Maybe [Action])
loadFile filename = do
  putStrLn filename
  input <- readFile filename
  let parsing = map (parse actionParser "") $ filter (/="") $ lines input
  let actions = map (\x -> case x of
                             Left _  -> Nothing
                             Right a -> Just a) parsing
  return $ sequence actions

-- | Initial text on the interpreter.
initText :: String
initText = unlines [
  formatIntro ++ "Welcome to the Mikrokosmos Lambda Interpreter!" ++ end,
  formatFormula ++ "Version 0.1.0. GNU General Public License Version 3." ++ end
  ]

-- | Parses an interpreter action.
interpreteractionParser :: Parser InterpreterAction
interpreteractionParser = choice [ try interpretParser
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
actionParser = choice [ try bindParser
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

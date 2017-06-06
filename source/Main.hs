module Main where

import           Control.Monad.Trans
import           Control.Monad.State
import           Control.Exception
import           System.Console.Haskeline
import           Text.ParserCombinators.Parsec hiding (try)
import           Format
import           Interpreter
import           Environment
import           Options hiding (defaultOptions)


-- Lambda interpreter
-- The actions of the interpreter are written here. It allows to execute normal
-- actions (bindings and evaluation), and interpreter specific actions, as "quit"
-- or "load".


-- | Runs the interpreter with default settings and an empty context.
main :: IO ()
main =
  -- Uses the Options library, which requires the program to start with
  -- runCommand. The flags are stored in opts and other command line arguments
  -- are stored in args.
  runCommand $ \opts args -> do

  -- Reads the flags
  case flagVersion opts of
    True -> putStrLn versionText
    False ->
      case args of
        [] -> runInputT defaultSettings ( outputStrLn initialText
                                          >> interpreterLoop defaultEnv
                                        )
        [filename] -> executeFile filename
        _ -> putStrLn "Wrong number of arguments"


-- | Interpreter awaiting for an instruction.
interpreterLoop :: Environment -> InputT IO ()
interpreterLoop environment = do
  -- Gets the user input on the interpreter
  -- and parses it to a concrete action.
  minput <- getInputLine promptText
  let interpreteraction =
        case minput of
          Nothing -> Quit
          Just "" -> EmptyLine
          Just input -> case parse interpreteractionParser "" input of
            Left _  -> Error
            Right a -> a

  -- Executes the parsed action, every action may affect the
  -- context in a way, and returns the control to the interpreter. 
  case interpreteraction of
    -- Interprets an action
    Interpret action -> case runState (act action) environment of
                          (output, newenv) -> do
                            outputActions newenv output
                            interpreterLoop newenv

    -- Loads a file given the filename
    Load filename -> do
      maybeloadfile <- lift $ loadFile filename
      case maybeloadfile of
        Nothing -> do
          outputStrLn "Error loading file"
          interpreterLoop environment
        Just actions -> case runState (multipleAct actions) environment of
                          (output, newenv) -> do
                            outputActions newenv output
                            interpreterLoop newenv
    
    -- Ignores the empty line
    EmptyLine -> interpreterLoop environment
    
    -- Exists the interpreter
    Quit -> return ()

    -- Unknown command
    Error -> do
      outputStr formatFormula
      outputStrLn "Unknown command"
      outputStr end
      interpreterLoop environment

    -- Sets the verbose option
    SetVerbose -> do
      outputStrLn $
        formatFormula ++
        "verbose mode: " ++ if getVerbose environment then "off" else "on" ++
        end
      interpreterLoop (changeVerbose environment)
      
    -- Sets the color option
    SetColors -> interpreterLoop (changeColor environment)
    
    -- Prints the help
    Help -> outputStr helpText >> interpreterLoop environment




-- | Outputs results from actions. Given a list of options and outputs,
--   formats and prints them in console.
outputActions :: Environment -> [String] -> InputT IO ()
outputActions environment output = do
    outputStr formatFormula
    mapM_ (outputStr . format) output
    outputStr end
  where
    format :: String -> String
    format "" = ""
    format s
      | not (getVerbose environment) = (++"\n") . last . lines $ s
      | otherwise                    = s



-- Loading and reading files
-- | Loads the given filename and returns the complete list of actions.
--   Returns Nothing if there is an error reading or parsing the file.
loadFile :: String -> IO (Maybe [Action])
loadFile filename = do
  putStrLn filename
  input <- try $ (readFile filename) :: IO (Either IOException String)
  case input of
    Left _ -> return Nothing
    Right inputs -> do
      let parsing = map (parse actionParser "") $ filter (/="") $ lines inputs
      let actions = map (\x -> case x of
                                 Left _  -> Nothing
                                 Right a -> Just a) parsing
      return $ sequence actions

-- | Executes the commands inside a file. A .mkr file can contain a sequence of
--   expressions and variable bindings, and it is interpreted sequentially.
executeFile :: Filename -> IO ()
executeFile filename = do
  maybeloadfile <- loadFile filename
  case maybeloadfile of
    Nothing    -> putStrLn "Error loading file"
    Just actions -> case runState (multipleAct actions) defaultEnv of
                      (outputs, _) -> mapM_ (putStr . format) outputs
                      where
                        format :: String -> String
                        format "" = ""
                        format s = (++"\n") . last . lines $ s


-- Flags
-- | Flags datatype
data MainFlags = MainFlags
  { flagExec :: String
  , flagVersion :: Bool
  }

instance Options MainFlags where
  -- | Flags definition
  defineOptions = pure MainFlags
    <*> simpleOption "exec" ""
    "A file to execute and show its results"
    <*> simpleOption "version" False
    "Show program version"

module Main where

import           Control.Monad.Trans
import           Control.Monad.State
import           System.Environment
import           System.Console.Haskeline
import           Text.ParserCombinators.Parsec
import           Format
import           Interpreter

-- | A filename is a string containing the directory path and
-- the real name of the file.
type Filename = String


-- Lambda interpreter
-- The actions of the interpreter are written here. It allows to execute normal
-- actions (bindings and evaluation), and interpreter specific actions, as
-- "quit" or "load".

-- | Runs the interpreter with default settings and an empty context.
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runInputT defaultSettings (outputStrLn initialText
                                  >> interpreterLoop defaultOptions emptyContext)
    [filename] -> executeFile filename
    _ -> putStrLn "Wrong number of arguments"

-- TODO: State Monad
-- | Interpreter awaiting for an instruction.
interpreterLoop :: InterpreterOptions -> Context -> InputT IO ()
interpreterLoop options context = do
  minput <- getInputLine promptText
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
    Error -> do
      outputStr formatFormula
      outputStrLn "Unknown command"
      outputStr end
      interpreterLoop options context
    SetVerbose -> do
      outputStrLn $
        formatFormula ++
        "verbose mode: " ++ if getVerbose options then "off" else "on" ++
        end
      interpreterLoop (changeVerbose options) context
    SetColors  -> interpreterLoop (changeColor options) context
    Help -> outputStr helpText >> interpreterLoop options context
    Load filename -> do
      maybeloadfile <- lift $ loadFile filename
      case maybeloadfile of
        Nothing    -> outputStrLn "Error loading file"
        Just actions -> case runState (multipleAct actions) context of
                          (output, ccontext) -> do
                            outputActions options output
                            interpreterLoop options ccontext
    Interpret action -> case runState (act action) context of
                          (output, ccontext) -> do
                            outputActions options output
                            interpreterLoop options ccontext




-- | Outputs results from actions. Given a list of options and outputs,
-- formats and prints them in console.
outputActions :: InterpreterOptions -> [String] -> InputT IO ()
outputActions options output = do
    outputStr formatFormula
    mapM_ (outputStr . format) output
    outputStr end
  where
    format :: String -> String
    format "" = ""
    format s
      | not (getVerbose options) = (++"\n") . last . lines $ s
      | otherwise             = s




-- Loading and reading files
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

-- | Executes the commands inside a file. A .mkr file can contain a sequence of
--   expressions and variable bindings, and it is interpreted sequentially.
executeFile :: Filename -> IO ()
executeFile filename = do
  maybeloadfile <- loadFile filename
  case maybeloadfile of
    Nothing    -> putStrLn "Error loading file"
    Just actions -> case runState (multipleAct actions) emptyContext of
                      (outputs, _) -> mapM_ (putStr . format) outputs
                      where
                        format :: String -> String
                        format "" = ""
                        format s = (++"\n") . last . lines $ s

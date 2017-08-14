module Main where

import           Control.Monad.Trans
import           Control.Monad.State
import           Control.Exception
import           Data.List
import           System.Directory
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
  runCommand $ \opts args
  -- Reads the flags
   ->
    if flagVersion opts
      then putStrLn versionText
      else case args of
             [] ->
               runInputT
                 defaultSettings
                 (outputStrLn initialText >> interpreterLoop defaultEnv)
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
          Just input ->
            case parse interpreteractionParser "" input of
              Left _ -> Error
              Right a -> a
  -- Executes the parsed action, every action may affect the
  -- context in a way, and returns the control to the interpreter.
  case interpreteraction
    -- Interprets an action
        of
    Interpret action ->
      case runState (act action) environment of
        (output, newenv) -> do
          outputActions newenv output
          interpreterLoop newenv
    -- Loads a module and its dependencies given its name.
    -- Avoids repeated modules keeping only their first ocurrence.
    Load modulename -> do
      modules <- lift (nub <$> readAllModuleDepsRecursively [modulename])
      files <- lift $ mapM findFilename modules
      -- Concats all the module contents
      maybeactions <- fmap concat . sequence <$> lift (mapM loadFile files)
      case maybeactions of
        Nothing -> do
          outputStrLn "Error loading file"
          interpreterLoop environment
        Just actions ->
          case runState (multipleAct actions) environment of
            (output, newenv) -> do
              outputActions newenv output
              interpreterLoop newenv
    -- Ignores the empty line
    EmptyLine -> interpreterLoop environment
    -- Exits the interpreter
    Quit -> return ()
    -- Restarts the interpreter context
    Restart -> outputStrLn restartText >> interpreterLoop defaultEnv
    -- Unknown command
    Error -> outputStrLn errorUnknownCommand >> interpreterLoop environment
    -- Sets the verbose option
    SetVerbose setting ->
      setOption environment setting changeVerbose "verbose: "
    SetColor setting -> setOption environment setting changeColor "color mode: "
    SetSki setting -> setOption environment setting changeSkioutput "ski mode: "
    SetTypes setting -> setOption environment setting changeTypes "types: "
    SetTopo setting -> setOption environment setting changeTopo "topo mode: "
    -- Prints the help
    Help -> outputStr helpText >> interpreterLoop environment


-- | Sets the given option on/off.
setOption :: Environment -> Bool ->
             (Environment -> Bool -> Environment) ->
             String ->
             InputT IO ()
setOption environment setting change message = do
  outputStrLn $
    (if getColor environment then formatFormula else "") ++
    message ++ if setting then "on" else "off" ++ end  
  interpreterLoop (change environment setting)


-- | Outputs results from actions. Given a list of options and outputs,
--   formats and prints them in console.
outputActions :: Environment -> [String] -> InputT IO ()
outputActions environment output = do
    outputStr (if getColor environment then formatFormula else "")
    mapM_ (outputStr . format) output
    outputStr end
  where
    format = formatColor
    formatColor s
      | getColor environment = s
      | otherwise            = unlines $ map decolor $ lines s



-- Loading and reading files
-- | Loads the given filename and returns the complete list of actions.
--   Returns Nothing if there is an error reading or parsing the file.
loadFile :: String -> IO (Maybe [Action])
loadFile filename = do
  putStrLn $ formatLoading ++ "Loading " ++ filename ++ "..." ++ end
  input <- try $ readFile filename :: IO (Either IOException String)
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
    Nothing -> putStrLn "Error loading file"
    Just actions ->
      case runState (multipleAct actions) defaultEnv of
        (outputs, _) -> mapM_ (putStr . format) outputs
      where format :: String -> String
            format "" = ""
            format s = (++ "\n") . last . lines $ s


-- | Reads module dependencies
readFileDependencies :: Filename -> IO [Modulename]
readFileDependencies filename = do
  input <- try $ readFile filename :: IO (Either IOException String)
  case input of
    Left _ -> return []
    Right inputs -> return $
      map (drop 9) (filter (isPrefixOf "#INCLUDE ") $ filter (/="") $ lines inputs)

-- | Reads all the dependencies from a module list
readAllModuleDeps :: [Modulename] -> IO [Modulename]
readAllModuleDeps modulenames = do
  files <- mapM findFilename modulenames
  deps <- mapM readFileDependencies files
  return $ concat deps

-- | Read module dependencies recursively
readAllModuleDepsRecursively :: [Modulename] -> IO [Modulename]
readAllModuleDepsRecursively modulenames = do
  newmodulenames <- readAllModuleDeps modulenames
  let allmodulenames = nub (newmodulenames ++ modulenames)
  if modulenames == allmodulenames
    then return modulenames
    else readAllModuleDepsRecursively allmodulenames

-- | Given a module name, returns the filename associated with it
findFilename :: Modulename -> IO Filename
findFilename s = do
  appdir <- getAppUserDataDirectory "mikrokosmos"
  homedir <- getHomeDirectory

  -- Looks for the module in the common locations
  head <$> filterM doesFileExist
    [ "lib/" ++ s ++ ".mkr"
    , "./" ++ s ++ ".mkr"
    , appdir ++ "/" ++ s ++ ".mkr"
    , homedir ++ "/" ++ s ++ ".mkr"
    ]


-- Flags
-- | Flags datatype
data MainFlags = MainFlags
  { flagExec :: String
  , flagVersion :: Bool
  }

instance Options MainFlags where
  -- | Flags definition
  defineOptions = pure MainFlags
    <*> simpleOption "exec"    ""    "A file to execute and show its results"
    <*> simpleOption "version" False "Show program version"

module Main where

import           Control.Monad.Trans
import           Control.Monad.State
import           Control.Exception
import           Data.List
import           Data.Foldable (forM_)
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
  runCommand $ \opts args -> do
  -- Reads the libaries flag
  let initialEnv = if flagLibs opts then defaultEnv else librariesEnv
  -- Reads more flags
  if flagVersion opts
  then putStrLn versionText
  else case args of
             [] ->
               runInputT
                 defaultSettings
                 (outputStrLn initialText >> interpreterLoop initialEnv)
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
          Just "" -> Interpret EmptyLine
          Just input ->
            case parse interpreteractionParser "" (preformat input) of
              Left  _ -> Interpret Error
              Right a -> a

  newenvironment <- executeAction environment interpreteraction
  forM_ newenvironment interpreterLoop
              

-- | Executes the parsed action, every action may affect the context
-- in a way, and returns the control to the interpreter.
executeAction :: Environment -> InterpreterAction -> InputT IO (Maybe Environment)
executeAction environment interpreteraction = 
  case interpreteraction of
    -- Interprets an action
    Interpret action ->
      case runState (act action) environment of
        (output, newenv) -> do
          outputActions newenv output
          return $ Just newenv
    -- Loads a module and its dependencies given its name.
    -- Avoids repeated modules keeping only their first ocurrence.
    Load modulename -> do
      readallmoduledeps <- lift $ readAllModuleDepsRecursively [modulename]
      case readallmoduledeps of
        Nothing -> do
          outputStrLn errorNotFoundText
          return $ Just environment
        Just readallmodules -> do
          let modules = nub readallmodules
          files <- lift $ mapM findFilename modules
          -- Concats all the module contents
          case sequence files of
            Nothing -> do
              outputStrLn errorNotFoundText
              return $ Just environment
            Just allfiles -> do
              maybeactions <- fmap concat . sequence <$> lift (mapM loadFile allfiles)
              case maybeactions of
                Nothing -> do
                  outputStrLn "Error loading file"
                  return $ Just environment
                Just actions ->
                  case runState (multipleAct actions) environment of
                    (output, newenv) -> do
                      outputActions newenv output
                      return $ Just newenv
    -- Exits the interpreter
    Quit -> return Nothing



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
loadFile :: Filename -> IO (Maybe [Action])
loadFile filename = do
  putStrLn $ formatLoading ++ "Loading " ++ filename ++ "..." ++ end
  input <- try $ readFile filename :: IO (Either IOException String)
  case input of
    Left _ -> return Nothing
    Right inputs -> do
      let parsing = map (parse actionParser "" . preformat) . filter (/="") . lines $ inputs
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
        (outputs, _) -> mapM_ putStr outputs

-- | Reads module dependencies
readFileDependencies :: Filename -> IO [Modulename]
readFileDependencies filename = do
  input <- try $ readFile filename :: IO (Either IOException String)
  case input of
    Left _ -> return []
    Right inputs -> return $
      map (drop 9) (filter (isPrefixOf "#INCLUDE ") $ filter (/="") $ lines inputs)

-- | Reads all the dependencies from a module list.
--   Returns an error if a dependency cannot be found
readAllModuleDeps :: [Modulename] -> IO (Maybe [Modulename])
readAllModuleDeps modulenames = do
  files <- mapM findFilename modulenames
  deps <- mapM (mapM readFileDependencies) files
  return (concat <$> sequence deps)

-- | Read module dependencies recursively.
--   Returns an error if a dependency cannot be found
readAllModuleDepsRecursively :: [Modulename] -> IO (Maybe [Modulename])
readAllModuleDepsRecursively modulenames = do
  maybenewmodulenames <- readAllModuleDeps modulenames
  case maybenewmodulenames of
    Nothing -> return Nothing
    Just newmodulenames -> do
      let allmodulenames = nub (newmodulenames ++ modulenames)
      if modulenames == allmodulenames
      then return (Just modulenames)
      else readAllModuleDepsRecursively allmodulenames

-- | Given a module name, returns the filename associated with it
findFilename :: Modulename -> IO (Maybe Filename)
findFilename s = do
  appdir <- getAppUserDataDirectory "mikrokosmos"
  homedir <- getHomeDirectory

  -- Looks for the module in the common locations
  headMaybe <$> filterM doesFileExist
    [ "lib/" ++ s ++ ".mkr"
    , "./" ++ s ++ ".mkr"
    , appdir ++ "/" ++ s ++ ".mkr"
    , homedir ++ "/" ++ s ++ ".mkr"
    , "/usr/lib/mikrokosmos/" ++ s ++ ".mkr"
    ]
  where
    headMaybe [] = Nothing
    headMaybe (x:_) = Just x

-- Flags
-- | Flags datatype
data MainFlags = MainFlags
  { flagExec :: String
  , flagVersion :: Bool
  , flagLibs :: Bool
  }

instance Options MainFlags where
  -- | Flags definition
  defineOptions = pure MainFlags
    <*> simpleOption "exec"    ""    "A file to execute and show its results"
    <*> simpleOption "version" False "Show program version"
    <*> simpleOption "no-libs" False "Runs mikrokosmos without standard libraries"

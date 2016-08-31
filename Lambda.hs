module Lambda where

import           Control.Applicative           ((<$>), (<*>))
import           Control.Monad.Trans
import           Data.Char
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           System.Console.Haskeline
import           Text.ParserCombinators.Parsec

type Context = Map.Map String Exp



-- Parsing of Lambda Expressions.
-- The user can input a lambda expression with named variables, of
-- the form of "\x.x" or "(\a.(\b.a b))". The interpreter will parse
-- it into an internal representation.

-- | A lambda expression with named variables.
data LambdaName = LambdaVariable String               -- ^ variable
                | LambdaAbstraction String LambdaName -- ^ lambda abstraction 
                | Lapp LambdaName LambdaName          -- ^ function application

-- | Parses a lambda expression with named variables.
-- A lambda expression is a sequence of one or more autonomous
-- lambda expressions. They are parsed assuming left-associativity.
lambdaexp :: Parser LambdaName
lambdaexp = foldl1 Lapp <$> (spaces >> sepBy1 simpleexp spaces)

-- | Parses a simple lambda expression, without function applications
-- at the top level. It can be a lambda abstraction, a variable or another
-- potentially complex lambda expression enclosed in parentheses.
simpleexp :: Parser LambdaName
simpleexp = choice [lambdaabs, variable, parens lambdaexp]

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

-- | Parses a variable. Any name can form a lambda variable.
variable :: Parser LambdaName
variable = LambdaVariable <$> name

-- | Allowed variable names
name :: Parser String
name = many1 alphaNum

-- | Parses a lambda abstraction. The '\' is used as lambda. 
lambdaabs :: Parser LambdaName
lambdaabs = LambdaAbstraction <$> (char lambdachar >> name) <*> (char '.' >> lambdaexp)

-- | Char used to represent lambda in user's input.
lambdachar :: Char
lambdachar = '\\'

-- | Shows a lambda expression with named variables.
-- Parentheses are ignored; they are written only around applications.
showlexp :: LambdaName -> String
showlexp (LambdaVariable c)   = c
showlexp (LambdaAbstraction c e) = "λ" ++ c ++ "." ++ showlexp e ++ ""
showlexp (Lapp f g) = "(" ++ showlexp f ++ " " ++ showlexp g ++ ")"

instance Show LambdaName where
  show = showlexp




-- DeBruijn Expressions
-- The interpreter uses DeBruijn notation as an internal representation and
-- as output format. It is easier to do beta reduction with DeBruijn indexes.

-- | A lambda expression using DeBruijn indexes.
data Exp = Var Integer -- ^ integer indexing the variable.
         | Lambda Exp  -- ^ lambda abstraction
         | App Exp Exp -- ^ function application
         deriving (Eq)

-- | Translates a named variable expression into a DeBruijn one.
-- Uses a dictionary of already binded numbers and variables.
tobruijn :: Map.Map String Integer -- ^ dictionary of the names of the variables used
         -> Context                -- ^ dictionary of the names already binded on the scope
         -> LambdaName             -- ^ initial expression
         -> Exp
         
-- Every lambda abstraction is inserted in the variable dictionary,
-- and every number in the dictionary increases to reflect we are entering
-- into a deeper context.
tobruijn d context (LambdaAbstraction c e) = Lambda $ tobruijn newdict context e
  where newdict = Map.insert c 1 (Map.map succ d)
-- Translation of applications is trivial.
tobruijn d context (Lapp f g) = App (tobruijn d context f) (tobruijn d context g)
-- Every variable is checked on the variable dictionary and in the current scope.
tobruijn d context (LambdaVariable c) =
  case Map.lookup c d of
    Just n  -> Var n
    Nothing -> fromMaybe (Var 0) (Map.lookup c context)

-- | Transforms a lambda expression with named variables to a deBruijn index expression.
-- Uses only the dictionary of the variables in the current context.
toBruijn :: Context     -- ^ Variable context
         -> LambdaName  -- ^ Initial lambda expression with named variables
         -> Exp
toBruijn = tobruijn Map.empty

-- TODO: Show an index after the lambda
-- | Shows an expression with DeBruijn indexes.
showexp :: Exp -> String
showexp (Var n)    = show n
showexp (Lambda e) = "λ(" ++ showexp e ++ ")"
showexp (App f g)  = showexp f ++ " " ++ showexp g

instance Show Exp where
  show = showexp




-- Reductions of lambda expressions.

-- | Applies repeated simplification to the expression until it stabilizes and
-- returns the final simplified expression.
simplifyall :: Exp -> Exp
simplifyall = last . stepsSimplify

-- | Applies repeated simplification to the expression until it stabilizes and
-- returns all the intermediate results.
stepsSimplify :: Exp -> [Exp]
stepsSimplify e
  | e == s    = [e]
  | otherwise = e : stepsSimplify s
  where s = simplify e

-- TODO: Simplify internal operations first. This has not an optimal efficiency.
-- | Simplifies the expression recursively.
-- Applies only a beta reduction at each step.
simplify :: Exp -> Exp
simplify (Lambda e)         = Lambda (simplify e)
simplify (App (Lambda f) x) = betared (App (Lambda f) x)
simplify (App (Var e) x)    = App (Var e) (simplify x)
simplify (App (App f g) x)  = App (simplify (App f g)) x
simplify (Var e)            = Var e

-- | Applies beta-reduction to a function application.
-- Leaves the rest of the operations untouched.
betared :: Exp -> Exp
betared (App (Lambda f) x) = substitute 1 x f
betared e = e

-- | Substitutes an index for a lambda expression
substitute :: Integer -- ^ deBruijn index of the desired target
           -> Exp     -- ^ replacement for the index
           -> Exp     -- ^ initial expression
           -> Exp
substitute n x (Lambda e) = Lambda (substitute (succ n) x e)
substitute n x (App f g)  = App (substitute n x f) (substitute n x g)
substitute n x (Var m)
  | n == m    = x
  | otherwise = Var m




-- Lambda interpreter
-- The logic of the interpreter is written here. It allows to execute normal
-- actions (bindings and evaluation), and interpreter specific actions, as
-- "quit" or "load".
main :: IO ()
main = runInputT defaultSettings (outputStrLn initText >> interpreterLoop defaultOptions Map.empty)

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

-- | Language action. The language has a number of possible valid statements;
-- all on the following possible forms.
data Action = Bind (String, LambdaName) -- ^ bind a name to an expression
            | Execute LambdaName        -- ^ execute an expression
            | Comment                   -- ^ comment
            -- Derives Show for debugging purposes only. 
            deriving (Show)

-- | Executes a language action. Given a context and an action, returns
-- the new context after the action and a text output.
act :: Context -> Action -> (Context, String)
act context Comment       = (context,"")
act context (Bind (s,le)) = (Map.insert s (toBruijn context le) context, "")
act context (Execute le)  = (context,
                             unlines $
                              [ showlexp le ] ++
                              [ unlines $ map showexp $ stepsSimplify $ toBruijn context le ] ++
                              [ showexp $ simplifyall $ toBruijn context le]
                            )

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
prompt = "mikroλ> "

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
    Load filename -> do
      maybeloadfile <- lift $ loadFile filename
      case maybeloadfile of
        Nothing    -> outputStrLn "Error loading file"
        Just actions -> case multipleAct context actions of
                          (ccontext, outputs) -> do
                            outputActions options outputs
                            interpreterLoop options ccontext
    Interpret action -> case act context action of
                          (ccontext, output) -> do
                            outputActions options [output]
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
  "Welcome to the Mikrokosmos Lambda Interpreter!",
  "Version 0.1.0. GNU General Public License Version 3."
  ]

-- | Parses an interpreter action.
interpreteractionParser :: Parser InterpreterAction
interpreteractionParser = choice [ try interpretParser
                                 , try quitParser
                                 , try loadParser
                                 , try verboseParser
                                 ]

-- | Parses a language action as an interpreter action.
interpretParser :: Parser InterpreterAction
interpretParser = Interpret <$> actionParser

-- | Parses a language action.
actionParser :: Parser Action
actionParser = choice [try bindParser, try executeParser, try commentParser]

-- | Parses a binding between a variable an its representation.
bindParser :: Parser Action
bindParser = fmap Bind $ (,) <$> many1 alphaNum <*> (spaces >> char '=' >> spaces >> lambdaexp)

-- | Parses an expression in order to execute it.
executeParser :: Parser Action
executeParser = Execute <$> lambdaexp

-- | Parses comments.
commentParser :: Parser Action
commentParser = string "#" >> many anyChar >> return Comment

-- | Parses a "quit" command.
quitParser :: Parser InterpreterAction
quitParser = string ":quit" >> return Quit

-- | Parses a change in verbosity.
verboseParser :: Parser InterpreterAction
verboseParser = string ":verbose" >> return SetVerbose

-- | Parses a "load-file" command.
loadParser :: Parser InterpreterAction
loadParser = Load <$> (string ":load" >> between spaces spaces (many1 (satisfy (not . isSpace))))

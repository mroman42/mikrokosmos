-- Links:
-- http://dev.stephendiehl.com/fun/003_lambda_calculus.html
-- https://www.youtube.com/playlist?list=PLxj9UAX4Em-Ij4TKwKvo-SLp-Zbv-hB4B
-- Inspired by the Glambda interpreter

module Lambda where

{- TODO: Avoid lookup -}
{- TODO: Is it better to use Data.Map.Strict or Data.Map? -}
import           Control.Applicative           ((<$>), (<*>))
import           Control.Monad.Trans
import           Data.Char
import qualified Data.Map                      as Map
import           Data.Maybe
import           System.Console.Haskeline
import           Text.ParserCombinators.Parsec

type Context = Map.Map String Exp



-- Parsing of Lambda Expressions.
-- The user can input a lambda expression with named variables, of
-- the form of "\x.x" or "(\a.(\b.a b))". The interpreter will parse
-- it into an internal representation.

-- | A lambda expression with named variables.
data Lexp = Lvar String       -- ^ variable
          | Llam String Lexp  -- ^ lambda abstraction 
          | Lapp Lexp Lexp    -- ^ function application

-- | Parses a lambda expression with named variables.
-- A lambda expression is a sequence of one or more autonomous
-- lambda expressions. They are parsed assuming left-associativity.
lambdaexp :: Parser Lexp
lambdaexp = foldl1 Lapp <$> (spaces >> sepBy1 simpleexp spaces)

-- | Parses a simple lambda expression, without function applications
-- at the top level. It can be a lambda abstraction, a variable or another
-- potentially complex lambda expression enclosed in parentheses.
simpleexp :: Parser Lexp
simpleexp = choice [lambdaabs, variable, parens lambdaexp]

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

-- | Parses a variable. Any name can form a lambda variable.
variable :: Parser Lexp
variable = Lvar <$> name

-- | Allowed variable names
name :: Parser String
name = many1 alphaNum

-- | Parses a lambda abstraction. The '\' is used as lambda. 
lambdaabs :: Parser Lexp
lambdaabs = Llam <$> (char lambdachar >> name) <*> (char '.' >> lambdaexp)

-- | Char used to represent lambda in user's input.
lambdachar :: Char
lambdachar = '\\'

-- | Shows a lambda expression with named variables.
-- Parentheses are ignored; they are written only around applications.
showlexp :: Lexp -> String
showlexp (Lvar c)   = c
showlexp (Llam c e) = "λ" ++ c ++ "." ++ showlexp e ++ ""
showlexp (Lapp f g) = "(" ++ showlexp f ++ " " ++ showlexp g ++ ")"

instance Show Lexp where
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
         -> Lexp                   -- ^ initial expression
         -> Exp
         
-- Every lambda abstraction is inserted in the variable dictionary.
tobruijn d context (Llam c e) = Lambda $ tobruijn (Map.insert c 1 (Map.map succ d)) context e
-- Translation of applications is trivial.
tobruijn d context (Lapp f g) = App (tobruijn d context f) (tobruijn d context g)
-- Every variable is checked on the variable dictionary and in the current scope.
tobruijn d context (Lvar c) =
  case Map.lookup c d of
    Just n  -> Var n
    Nothing -> fromMaybe (Var 0) (Map.lookup c context)

-- | Transforms a lambda expression with named variables to a deBruijn index expression.
-- Uses only the dictionary of the variables in the current context.
toBruijn :: Context  -- ^ Variable context
         -> Lexp     -- ^ Initial lambda expression with named variables
         -> Exp
toBruijn = tobruijn Map.empty

-- | Shows an expression with DeBruijn indexes.
showexp :: Exp -> String
showexp (Var n)    = show n
showexp (Lambda e) = "λ(" ++ showexp e ++ ")"
showexp (App f g)  = showexp f ++ " " ++ showexp g

instance Show Exp where
  show = showexp




{- Reductions -}
-- | Applies simplification to the expression until it stabilizes.
simplifyall :: Exp -> Exp
simplifyall e
  | e == s    = e
  | otherwise = simplifyall s
  where s = simplify e

simplify :: Exp -> Exp
simplify (Lambda e) = Lambda (simplify e)
simplify (App f g)  = betared (App (simplify f) (simplify g))
simplify (Var e)    = Var e


-- | Applies beta-reduction to an expression.
betared :: Exp -- ^ initial expression
        -> Exp
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



{- TODO: Better interaction (:load)-}
data InterpreterAction = Interpret Action
                       | EmptyLine
                       | Error
                       | Quit
                       | Load String

data Action = Bind (String, Lexp)
            | Execute Lexp
            | Comment
            deriving (Show)

main :: IO ()
main = runInputT defaultSettings (outputStrLn initText >> interpreterLoop Map.empty)



interpreterLoop :: Context -> InputT IO ()
interpreterLoop context = do
  minput <- getInputLine "mikroλ> "
  let interpreteraction =
        case minput of
          Nothing -> Quit
          Just "" -> EmptyLine
          Just input -> case parse interpreteractionParser "" input of
            Left _  -> Error
            Right a -> a
  case interpreteraction of
    EmptyLine -> interpreterLoop context
    Quit -> return ()
    Error -> outputStrLn "Error"
    Load filename -> do
      maybeloadfile <- lift $ loadFile filename
      case maybeloadfile of
        Nothing    -> outputStrLn "Error loading file"
        Just actions -> case multipleAct context actions of
                          (ccontext, output) -> outputStr output >> interpreterLoop ccontext
    Interpret action -> case act context action of
                          (ccontext, output) -> outputStr output >> interpreterLoop ccontext


act :: Context -> Action -> (Context, String)
act context Comment       = (context,"")
act context (Bind (s,le)) = (Map.insert s (toBruijn context le) context, "")
act context (Execute le)  = (context, unlines [ showlexp le
                                                , showexp $ toBruijn context le
                                                , showexp $ simplifyall $ toBruijn context le
                                                ])

-- TODO: Writer monad
-- TODO: Use Text instead of String for efficiency
multipleAct :: Context -> [Action] -> (Context, String)
multipleAct context = foldl (\(ccontext,text) action ->
                                (fst $ act ccontext action, text ++ snd (act ccontext action)))
                      (context,"")

loadFile :: String -> IO (Maybe [Action])
loadFile filename = do
  putStrLn filename
  input <- readFile filename
  let parsing = map (parse actionParser "") $ filter (/="") $ lines input
  let actions = map (\x -> case x of
                             Left _  -> Nothing
                             Right a -> Just a) parsing
  return $ sequence actions

initText :: String
initText = "Welcome to the Mikrokosmos Lambda Interpreter!"

interpreteractionParser :: Parser InterpreterAction
interpreteractionParser = choice [try interpretParser, try quitParser, try loadParser]

interpretParser :: Parser InterpreterAction
interpretParser = Interpret <$> actionParser

actionParser :: Parser Action
actionParser = choice [try bindParser, try executeParser, try commentParser]

bindParser :: Parser Action
bindParser = fmap Bind $ (,) <$> many1 alphaNum <*> (spaces >> char '=' >> spaces >> lambdaexp)

executeParser :: Parser Action
executeParser = Execute <$> lambdaexp

commentParser :: Parser Action
commentParser = string "#" >> many anyChar >> return Comment

quitParser :: Parser InterpreterAction
quitParser = string ":quit" >> return Quit

loadParser :: Parser InterpreterAction
loadParser = Load <$> (string ":load" >> between spaces spaces (many1 (satisfy (not . isSpace))))

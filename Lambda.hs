-- Links:
-- http://dev.stephendiehl.com/fun/003_lambda_calculus.html
-- https://www.youtube.com/playlist?list=PLxj9UAX4Em-Ij4TKwKvo-SLp-Zbv-hB4B
-- Inspired by the Glambda interpreter

module Lambda where

{- Lambda parsing -}
{- TODO: Avoid lookup -}
{- TODO: Is it better to use Data.Map.Strict or Data.Map? -}
import Text.ParserCombinators.Parsec
import Control.Applicative ((<$>),(<*>))
import qualified Data.Map as Map
import System.Console.Haskeline
import Data.Maybe

-- | A lambda expression with named variables.
data Lexp = Lvar String
          | Llam String Lexp
          | Lapp Lexp Lexp
          deriving (Show)

-- | Parses a lambda expression with named variables.
lambdaexp :: Parser Lexp
lambdaexp = foldl1 Lapp <$> (spaces >> sepBy1 simpleexp spaces)

simpleexp :: Parser Lexp
simpleexp = choice [lambdaabs, variable, parens lambdaexp]

variable :: Parser Lexp
variable = Lvar <$> many1 lower

lambdaabs :: Parser Lexp 
lambdaabs = Llam <$> (char '\\' >> many1 lower) <*> (char '.' >> lambdaexp)

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

-- | Shows a lambda expression with named variables
showlexp :: Lexp -> String
showlexp (Lvar c)   = c
showlexp (Llam c e) = "λ" ++ c ++ "." ++ showlexp e ++ ""
showlexp (Lapp f g) = showlexp f ++ " " ++ showlexp g



{- Lambda Expressions DeBruijn -}
-- | A lambda expression using DeBruijn indexes.
data Exp = Var Integer
         | Lambda Exp
         | App Exp Exp
         deriving (Eq)

showexp :: Exp -> String
showexp (Var n)    = show n
showexp (Lambda e) = "λ(" ++ showexp e ++ ")"
showexp (App f g)  = showexp f ++ " " ++ showexp g 

instance Show Exp where
  show = showexp

{- Translation to DeBruijn -}
tobruijn :: Map.Map String Integer -> Map.Map String Exp -> Lexp -> Exp
tobruijn d context (Llam c e) = Lambda $ tobruijn (Map.insert c 1 (Map.map succ d)) context e
tobruijn d context (Lapp f g) = App (tobruijn d context f) (tobruijn d context g)
tobruijn d context (Lvar c) =
  case Map.lookup c d of
    Just n  -> Var n
    Nothing -> fromMaybe (Var 0) (Map.lookup c context)

-- | Transforms a lambda expression with named variables to a deBruijn index expression
toBruijn :: Map.Map String Exp -- ^ Variable context
         -> Lexp               -- ^ Initial lambda expression with named variables
         -> Exp
toBruijn = tobruijn Map.empty

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
data Action = Interpret Lexp
            | Bind (String, Lexp)
            | EmptyLine
            | Quit
              
main :: IO ()
main = runInputT defaultSettings (outputStrLn initText >> interpreterLoop Map.empty)
  
interpreterLoop :: Map.Map String Exp -> InputT IO ()
interpreterLoop context = do
  minput <- getInputLine "mikroλ> "
  case minput of
      Nothing -> return ()
      Just ":quit" -> return ()
      Just input -> case parse lambdaexp "" input of
        Left _  -> outputStrLn "Error" 
        Right s -> outputStrLn (showlexp s)
                   >> (outputStrLn . showexp $ toBruijn context s)
                   >> (outputStrLn . showexp $ simplifyall $ toBruijn context s)
                   >> interpreterLoop context

initText :: String
initText = "Welcome to the Mikrokosmos Lambda Interpreter!"

bindParser :: Parser Action
bindParser = fmap Bind $ (,) <$> many1 letter <*> (spaces >> char '=' >> spaces >> lambdaexp)

interpretParser :: Parser Action
interpretParser = fmap Interpret lambdaexp

quitParser :: Parser Action
quitParser = string "quit" >> return Quit

actionParser :: Parser Action
actionParser = choice [try bindParser]

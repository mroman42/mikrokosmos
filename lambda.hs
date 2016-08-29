-- Links:
-- http://dev.stephendiehl.com/fun/003_lambda_calculus.html

{- Lambda parsing -}
{- TODO: \x.\y.x y z -}
{- TODO: Avoid lookup -}
{- TODO: Is it better to use Data.Map.Strict or Data.Map? -}
import Text.ParserCombinators.Parsec
import qualified Data.Map as Map

data Lexp = Lvar Char
          | Llam Char Lexp
          | Lapp Lexp Lexp
          deriving (Show)

lambdaexp :: Parser Lexp
lambdaexp = choice [lambdaabs, try lambdaapp, parens lambdaexp, variable]

variable :: Parser Lexp
variable = fmap Lvar letter

lambdaabs :: Parser Lexp
lambdaabs = do
  l <- char '\\'
  var <- anyChar
  p <- char '.'
  exp <- lambdaexp
  return $ Llam var exp

lambdaapp :: Parser Lexp
lambdaapp = parens $ do
  exp1 <- lambdaexp
  i    <- skipMany space
  exp2 <- lambdaexp
  return $ Lapp exp1 exp2

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

showlexp :: Lexp -> String
showlexp (Lvar c) = [c]
showlexp (Llam c e) = "λ" ++ [c] ++ "." ++ showlexp e ++ ""
showlexp (Lapp f g) = showlexp f ++ " " ++ showlexp g

{- Lambda Expressions DeBruijn -}
data Exp = Var Integer
         | Lambda Exp
         | App Exp Exp

showexp :: Exp -> String
showexp (Var n)    = show n
showexp (Lambda e) = "λ(" ++ showexp e ++ ")"
showexp (App f g)  = showexp f ++ " " ++ showexp g 

instance Show Exp where
  show = showexp

{- Translation to DeBruijn -}
tobruijn :: Map.Map Char Integer -> Lexp -> Exp
tobruijn d (Llam c e) = Lambda $ tobruijn (Map.insert c 1 (Map.map succ d)) e
tobruijn d (Lapp f g) = App (tobruijn d f) (tobruijn d g)
tobruijn d (Lvar c) =
  case Map.lookup c d of
    Just n  -> Var n
    Nothing -> Var 0

toBruijn = tobruijn Map.empty

{- Reductions -}
betared :: Exp -> Exp
betared (App (Lambda f) x) = substitute 1 x f
betared e = e

substitute :: Integer -> Exp -> Exp -> Exp
substitute n x (Lambda e) = Lambda (substitute (succ n) x e)
substitute n x (App f g)  = App (substitute n x f) (substitute n x g)
substitute n x (Var m)
  | n == m    = x
  | otherwise = (Var m)



{- TODO: Better interaction (:quit,:load)-}
main :: IO ()
main = do
  l <- getLine
  case parse lambdaexp "" l of
        Left e  -> putStrLn "Error" 
        Right s -> (putStrLn $ showlexp s)
                   >> (putStrLn . showexp $ toBruijn s)
                   >> (putStrLn . showexp $ betared $ toBruijn s)
                   >> main

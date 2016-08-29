-- Links:
-- http://dev.stephendiehl.com/fun/003_lambda_calculus.html

{- Lambda parsing -}
import Text.ParserCombinators.Parsec

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
showlexp (Llam c e) = "λx." ++ showlexp e ++ ""
showlexp (Lapp f g) = showlexp f ++ " " ++ showlexp g

{- Lambda Expressions DeBrunjin -}
data Exp = Var Integer
         | Lambda Exp
         | App Exp Exp

showexp :: Exp -> String
showexp (Var n)    = show n
showexp (Lambda e) = "λ(" ++ showexp e ++ ")"
showexp (App f g)  = showexp f ++ " " ++ showexp g 

instance Show Exp where
  show = showexp




main :: IO ()
main = do
  l <- getLine
  case parse lambdaexp "" l of
        Left e  -> putStrLn "Error" 
        Right s -> (putStrLn $ showlexp s) >> main
        
main' = interact (unlines . map (\xs ->
                    case (parse lambdaexp "" xs) of
                      Left _ -> undefined
                      Right s -> showlexp s
                      )
                . lines)

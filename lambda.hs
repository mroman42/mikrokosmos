-- Links:
-- http://dev.stephendiehl.com/fun/003_lambda_calculus.html

{- Lambda parsing -}
import Text.Parsec
import Text.ParserCombinators.Parsec

data Lexp = Lvar Char
          | Llam Char Lexp
          | Lapp Lexp Lexp
          deriving (Show)

lambdaexp :: Parser Lexp
lambdaexp = choice [lambdaabs, lambdaapp, parens lambdaexp, variable]

variable :: Parser Lexp
variable = fmap Lvar anyChar

lambdaabs :: Parser Lexp
lambdaabs = do
  l <- char '\\'
  var <- anyChar
  p <- char '.'
  exp <- lambdaexp
  return $ Llam var exp

lambdaapp :: Parser Lexp
lambdaapp = do
  exp1 <- parens lambdaexp
  exp2 <- parens lambdaexp
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
main = interact (unlines . map (\xs ->
                    case (parse lambdaexp "" xs) of
                      Left _ -> undefined
                      Right s -> showlexp s
                      )
                . lines)

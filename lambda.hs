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

main = putStrLn $ show (Lambda (Var 1))

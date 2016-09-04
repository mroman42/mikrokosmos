module Lambda
  ( Exp (Var, Lambda, App)
  , simplifyall
  , stepsSimplify
  )
where

-- DeBruijn Expressions
-- The interpreter uses DeBruijn notation as an internal representation and
-- as output format. It is easier to do beta reduction with DeBruijn indexes.

-- | A lambda expression using DeBruijn indexes.
data Exp = Var Integer -- ^ integer indexing the variable.
         | Lambda Exp  -- ^ lambda abstraction
         | App Exp Exp -- ^ function application
         deriving (Eq, Ord)

instance Show Exp where
  show = showexp


-- TODO: Show an index after the lambda
-- | Shows an expression with DeBruijn indexes.
showexp :: Exp -> String
showexp (Var n)    = show n
showexp (Lambda e) = "λ(" ++ showexp e ++ ")"
showexp (App f g)  = "(" ++ showexp f ++ " " ++ showexp g ++ ")"




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
betared (App (Lambda e) x) = substitute 1 x e
betared e = e

-- | Substitutes an index for a lambda expression
substitute :: Integer -- ^ deBruijn index of the desired target
           -> Exp     -- ^ replacement for the index
           -> Exp     -- ^ initial expression
           -> Exp
substitute n x (Lambda e) = Lambda (substitute (succ n) (incrementFreeVars 0 x) e)
substitute n x (App f g)  = App (substitute n x f) (substitute n x g)
substitute n x (Var m)
  -- The lambda is replaced directly
  | n == m    = x
  -- A more exterior lambda decreases a number
  | n <  m    = Var (m-1)
  -- An unrelated variable remains untouched
  | otherwise = Var m

-- | Increments free variables assuming they are bind to an
-- external lambda. This is done to substitute them correctly in
-- internal expressions.
incrementFreeVars :: Integer -> Exp -> Exp
incrementFreeVars n (App f g)  = App (incrementFreeVars n f) (incrementFreeVars n g)
incrementFreeVars n (Lambda e) = Lambda (incrementFreeVars (succ n) e)
incrementFreeVars n (Var m)
  | m > n     = Var (succ m)
  | otherwise = Var m

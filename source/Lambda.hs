{-|
Module: Lambda
Description: DeBruijn lambda expressions.
License: GPL-3

This module deals with the parsing, reduction and printing of lambda
expressions using DeBruijn notation. The interpreter uses DeBruijn
notation as an internal representation and as output format. This is because
it is easier to do beta reduction with DeBruijn indexes.
-}

module Lambda
  ( Exp (Var, Lambda, App, Pair, Pi1, Pi2, Inl, Inr, Caseof, Unit, Abort, Absurd)
  , simplifyAll
  , simplifySteps
  , showReduction
  , usestypecons
  , isOpenExp
  )
where

import Format

-- DeBruijn Expressions
-- | A lambda expression using DeBruijn indexes.
data Exp = Var Integer        -- ^ integer indexing the variable.
         | Lambda Exp         -- ^ lambda abstraction.
         | App Exp Exp        -- ^ function application.
         | Pair Exp Exp       -- ^ typed pair of expressions.
         | Pi1 Exp            -- ^ typed first projection.
         | Pi2 Exp            -- ^ typed second projection.
         | Inl Exp            -- ^ typed left injection.
         | Inr Exp            -- ^ typed right injection.
         | Caseof Exp Exp Exp -- ^ typed case of.
         | Unit               -- ^ typed unit element.
         | Abort Exp          -- ^ typed abort derivation.
         | Absurd Exp         -- ^ typed absurd derivation.
         deriving (Eq, Ord)

instance Show Exp where
  show = showexp

-- | Shows an expression with DeBruijn indexes.
showexp :: Exp -> String
showexp (Var n)        = show n
showexp (Lambda e)     = "λ" ++ showexp e ++ ""
showexp (App f g)      = "(" ++ showexp f ++ " " ++ showexp g ++ ")"
showexp (Pair a b)     = "(" ++ showexp a ++ "," ++ showexp b ++ ")"
showexp (Pi1 m)        = "(" ++ "FST " ++ showexp m ++ ")"
showexp (Pi2 m)        = "(" ++ "SND " ++ showexp m ++ ")"
showexp (Inl m)        = "(" ++ "INL " ++ showexp m ++ ")"
showexp (Inr m)        = "(" ++ "INR " ++ showexp m ++ ")"
showexp (Caseof m n p) = "(" ++ "CASE " ++ showexp m ++ " OF " ++ showexp n ++ "; " ++ showexp p ++ ")"
showexp Unit         = "*"
showexp (Abort a)      = "(ABORT " ++ showexp a ++ ")"
showexp (Absurd a)     = "(ABSURD " ++ showexp a ++ ")"

-- | Shows an expression coloring the next reduction.
showReduction :: Exp -> String
showReduction (Lambda e)         = "λ" ++ showReduction e
showReduction (App (Lambda f) x) = betaColor (App (Lambda f) x)
showReduction (Var e)            = show e
showReduction (App rs x)         = "(" ++ showReduction rs ++ " " ++ showReduction x ++ ")"
showReduction e                  = show e


-- | Colors a beta reduction
betaColor :: Exp -> String
betaColor (App (Lambda e) x) =
  "(" ++
  formatSubs1 ++ "λ" ++ formatFormula ++
  indexColor 1 e ++
  " " ++
  formatSubs2 ++ showexp x ++ formatFormula
  ++ ")"
betaColor e = show e

-- | Colors all the appearances of a given index
indexColor :: Integer -> Exp -> String
indexColor n (Lambda e) = "λ" ++ indexColor (succ n) e
indexColor n (App f g)  = "(" ++ indexColor n f ++ " " ++ indexColor n g ++ ")"
indexColor n (Var m)
  | n == m    = formatSubs1 ++ show m ++ formatFormula
  | otherwise = show m
indexColor _ e = show e



-- Reductions of lambda expressions.

-- | Applies repeated simplification to the expression until it stabilizes and
--   returns the final simplified expression.
--
-- >>> simplifyAll $ App (Lambda (Var 1)) (Lambda (Var 1))
-- λ1
--
simplifyAll :: String -> Exp -> Exp
simplifyAll strategy = last . (simplifySteps strategy)

-- | Applies repeated simplification to the expression until it stabilizes and
-- returns all the intermediate results.
--
-- >>> simplifySteps $ App (Lambda (Var 1)) (Lambda (Var 1))
-- [(λ1 λ1),λ1]
--
simplifySteps :: String -> Exp -> [Exp]
simplifySteps strategy e
  | e == s    = [e]
  | otherwise = e : simplifySteps strategy s
  where s = case strategy of "full" -> simplify e 
                             "cbv" -> simplify_cbv e
                             _ -> simplify_cbn e

-- | Simplifies the expression recursively.
-- Applies only one parallel beta reduction at each step.
simplify :: Exp -> Exp
simplify (Lambda e)           = Lambda (simplify e)
simplify (App (Lambda f) x)   = betared (App (Lambda f) x)
simplify (App (Var e) x)      = App (Var e) (simplify x)
simplify (App (App f g) x)    = App (simplify (App f g)) (simplify x)
simplify (App a b)            = App (simplify a) (simplify b)
simplify (Var e)              = Var e
simplify (Pair a b)           = Pair (simplify a) (simplify b)
simplify (Pi1 (Pair a _))     = a
simplify (Pi1 m)              = Pi1 (simplify m)
simplify (Pi2 (Pair _ b))     = b
simplify (Pi2 m)              = Pi2 (simplify m)
simplify (Inl m)              = Inl (simplify m)
simplify (Inr m)              = Inr (simplify m)
simplify (Caseof (Inl m) a _) = App a m
simplify (Caseof (Inr m) _ b) = App b m
simplify (Caseof a b c)       = Caseof (simplify a) (simplify b) (simplify c)
simplify Unit               = Unit
simplify (Abort a)            = Abort (simplify a)
simplify (Absurd a)           = Absurd (simplify a)

-- | Simplifies the expression recursively using call by name strategy.
-- Applies only one parallel beta reduction at each step.
simplify_cbn :: Exp -> Exp
simplify_cbn (Lambda e)           = Lambda e
simplify_cbn (App (Lambda f) x)   = betared (App (Lambda f) x)
simplify_cbn (App (Var e) x)      = App (Var e) (simplify_cbn x)
simplify_cbn (App (App f g) x)    = App (simplify_cbn (App f g)) (simplify_cbn x)
simplify_cbn (App a b)            = App (simplify_cbn a) (simplify_cbn b)
simplify_cbn (Var e)              = Var e
simplify_cbn (Pair a b)           = Pair (simplify_cbn a) (simplify_cbn b)
simplify_cbn (Pi1 (Pair a _))     = a
simplify_cbn (Pi1 m)              = Pi1 (simplify_cbn m)
simplify_cbn (Pi2 (Pair _ b))     = b
simplify_cbn (Pi2 m)              = Pi2 (simplify_cbn m)
simplify_cbn (Inl m)              = Inl (simplify_cbn m)
simplify_cbn (Inr m)              = Inr (simplify_cbn m)
simplify_cbn (Caseof (Inl m) a _) = App a m
simplify_cbn (Caseof (Inr m) _ b) = App b m
simplify_cbn (Caseof a b c)       = Caseof (simplify_cbn a) (simplify_cbn b) (simplify_cbn c)
simplify_cbn Unit                 = Unit
simplify_cbn (Abort a)            = Abort (simplify_cbn a)
simplify_cbn (Absurd a)           = Absurd (simplify_cbn a)

-- | Check if a expression is already in normal form which
-- should be equivalent to the notion of being a value.
is_value :: Exp -> Bool
is_value (Lambda _)           = True
is_value (App (Lambda _) _)   = False
is_value (App (Var _) x)      = is_value x
is_value (App a b)            = is_value a && is_value b
is_value (Var _)              = True
is_value (Pair a b)           = is_value a && is_value b
is_value (Pi1 (Pair _ _))     = False
is_value (Pi1 m)              = is_value m
is_value (Pi2 (Pair _ _))     = False
is_value (Pi2 m)              = is_value m
is_value (Inl m)              = is_value m
is_value (Inr m)              = is_value m
is_value (Caseof (Inl _) _ _) = False 
is_value (Caseof (Inr _) _ _) = False
is_value (Caseof a b c)       = is_value a && is_value b && is_value c
is_value Unit                 = True
is_value (Abort a)            = is_value a
is_value (Absurd a)           = is_value a

-- | Simplifies the expression recursively using call by value strategy.
-- Applies only one parallel beta reduction at each step.
simplify_cbv :: Exp -> Exp
simplify_cbv (Lambda e)           = Lambda e
simplify_cbv (App (Lambda f) x)   = if (is_value x) 
                                    then betared (App (Lambda f) x)
                                    else betared (App (Lambda f) (simplify_cbv x))
simplify_cbv (App (Var e) x)      = App (Var e) (simplify_cbv x)
simplify_cbv (App (App f g) x)    = App (simplify_cbv (App f g)) (simplify_cbv x)
simplify_cbv (App a b)            = App (simplify_cbv a) (simplify_cbv b)
simplify_cbv (Var e)              = Var e
simplify_cbv (Pair a b)           = Pair (simplify_cbv a) (simplify_cbv b)
simplify_cbv (Pi1 (Pair a _))     = a
simplify_cbv (Pi1 m)              = Pi1 (simplify_cbv m)
simplify_cbv (Pi2 (Pair _ b))     = b
simplify_cbv (Pi2 m)              = Pi2 (simplify_cbv m)
simplify_cbv (Inl m)              = Inl (simplify_cbv m)
simplify_cbv (Inr m)              = Inr (simplify_cbv m)
simplify_cbv (Caseof (Inl m) a _) = App a m
simplify_cbv (Caseof (Inr m) _ b) = App b m
simplify_cbv (Caseof a b c)       = Caseof (simplify_cbv a) (simplify_cbv b) (simplify_cbv c)
simplify_cbv Unit                 = Unit
simplify_cbv (Abort a)            = Abort (simplify_cbv a)
simplify_cbv (Absurd a)           = Absurd (simplify_cbv a)

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
substitute n x (Pair a b) = Pair (substitute n x a) (substitute n x b)
substitute n x (Pi1 a) = Pi1 (substitute n x a)
substitute n x (Pi2 a) = Pi2 (substitute n x a)
substitute n x (Inl a) = Inl (substitute n x a)
substitute n x (Inr a) = Inr (substitute n x a)
substitute n x (Caseof a b c) = Caseof (substitute n x a) (substitute n x b) (substitute n x c)
substitute _ _ Unit = Unit
substitute n x (Abort a) = Abort (substitute n x a)
substitute n x (Absurd a) = Absurd (substitute n x a)
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
incrementFreeVars n (Pair a b) = Pair (incrementFreeVars n a) (incrementFreeVars n b)
incrementFreeVars n (Pi1 a)    = Pi1 (incrementFreeVars n a)
incrementFreeVars n (Pi2 a)    = Pi2 (incrementFreeVars n a)
incrementFreeVars n (Inl a)    = Inl (incrementFreeVars n a)
incrementFreeVars n (Inr a)    = Inr (incrementFreeVars n a)
incrementFreeVars n (Caseof a b c) = Caseof (incrementFreeVars n a) (incrementFreeVars n b) (incrementFreeVars n c)
incrementFreeVars _ Unit    = Unit
incrementFreeVars n (Abort a) = Abort (incrementFreeVars n a)
incrementFreeVars n (Absurd a) = Absurd (incrementFreeVars n a)


-- | Determines if the given variable is free on the expression.
-- freein :: Integer -> Exp -> Bool
-- freein n (Var m)    = n == m
-- freein n (Lambda e) = freein (succ n) e
-- freein n (App u v)  = (freein n u) && (freein n v)

-- | Returns true if the expression has at least one type constructor.
usestypecons :: Exp -> Bool
usestypecons (Var _) = False
usestypecons (App a b) = usestypecons a || usestypecons b
usestypecons (Lambda b) = usestypecons b
usestypecons _ = True


-- | Returns true if the expression contains open undefined variables.
isOpenExp :: Exp -> Bool
isOpenExp (Var n) = n == 0
isOpenExp (App a b) = isOpenExp a || isOpenExp b
isOpenExp (Lambda a) = isOpenExp a
isOpenExp (Pair a b) = isOpenExp a || isOpenExp b
isOpenExp (Pi1 a) = isOpenExp a
isOpenExp (Pi2 a) = isOpenExp a
isOpenExp (Inl a) = isOpenExp a
isOpenExp (Inr a) = isOpenExp a
isOpenExp (Caseof a b c) = isOpenExp a || isOpenExp b || isOpenExp c
isOpenExp Unit = False
isOpenExp (Abort a) = isOpenExp a
isOpenExp (Absurd a) = isOpenExp a

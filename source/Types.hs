module Types
  ( Type (Tvar, Arrow)
  , typeinfer
  , typeinference
  )
where

import Control.Monad
import Lambda

import qualified Data.Map as Map

type Context      = Map.Map Integer Type
type Variable     = Integer
type Substitution = Type -> Type
data Type         = Tvar Variable | Arrow Type Type

instance Show Type where
  show (Tvar t)                  = typevariableNames !! (fromInteger t)
  show (Arrow (Tvar x) (Tvar y)) = show (Tvar x) ++ " -> "  ++ show (Tvar y)
  show (Arrow (Tvar x) b       ) = show (Tvar x) ++ " -> "  ++ show b
  show (Arrow a        (Tvar y)) = "(" ++ show a ++ ") -> " ++ show (Tvar y)
  show (Arrow a        b       ) = "(" ++ show a ++ ") -> " ++ show b

subs :: Variable -> Type -> Substitution
subs x typ (Tvar y)
  | x == y    = typ
  | otherwise = Tvar y
subs x typ (Arrow a b) = Arrow (subs x typ a) (subs x typ b)

occurs :: Variable -> Type -> Bool
occurs x (Tvar y)    = x == y
occurs x (Arrow a b) = occurs x a || occurs x b

unify :: Type -> Type -> Maybe Substitution
unify (Tvar x) (Tvar y)
  | x == y    = Just id
  | otherwise = Just (subs x (Tvar y))
unify (Tvar x) b
  | occurs x b = Nothing
  | otherwise  = Just (subs x b)
unify a (Tvar y)
  | occurs y a = Nothing
  | otherwise  = Just (subs y a)
unify (Arrow a b) (Arrow c d) = do
  p <- unify b d
  q <- unify (p a) (p c)
  return (q . p)

applyctx :: Substitution -> Context -> Context
applyctx = Map.map

emptyctx :: Context
emptyctx = Map.empty

-- incrementvars :: Type -> Type
-- incrementvars (Tvar n)    = Tvar (succ n)
-- incrementvars (Arrow a b) = Arrow (incrementvars a) (incrementvars b)

incrementindices :: Context -> Context
incrementindices = Map.mapKeys succ

-- typeinfer :: Context -> Exp -> Type -> Maybe Substitution

-- typeinfer ctx (App p q) b = do
--   let nctx = applyctx incrementvars ctx
--   let nb   = incrementvars b
--   sigma <- typeinfer nctx                  p (Arrow (Tvar 0) nb)
--   tau   <- typeinfer (applyctx sigma nctx) q (sigma (Tvar 0))
--   return (tau . sigma)
  
-- typeinfer ctx (Lambda p) b = do
--   let nctx = applyctx incrementvars $ applyctx incrementvars ctx
--   let nb   = incrementvars $ incrementvars b
--   sigma <- unify nb (Arrow (Tvar 0) (Tvar 1))
--   let nnctx = applyctx sigma (Map.insert 1 (Tvar 0) (incrementindices nctx))
--   tau   <- typeinfer nnctx p (sigma (Tvar 1))
--   return (tau . sigma)
  
-- typeinfer ctx (Var x) b
--   | Map.member x ctx = do
--       var <- Map.lookup x ctx
--       unify var b
--   | otherwise  = Nothing

typeinfer :: [Variable] -> Context -> Exp -> Type -> Maybe Substitution
typeinfer [] _ _ _ = Nothing
typeinfer [_] _ _ _ = Nothing

typeinfer _ ctx (Var n) b
   | Map.member n ctx = do
       var <- Map.lookup n ctx
       unify var b
   | otherwise  = Nothing

typeinfer (x:vars) ctx (App p q) b = do
  sigma <- typeinfer (odds  vars) ctx                  p (Arrow (Tvar x) b)
  tau   <- typeinfer (evens vars) (applyctx sigma ctx) q (sigma (Tvar x))
  return (tau . sigma)
  where
    odds [] = []
    odds [_] = []
    odds (_:e:xs) = e : odds xs
    evens [] = []
    evens [a] = [a]
    evens (e:_:xs) = e : evens xs

typeinfer (a:x:vars) ctx (Lambda p) b = do
  sigma <- unify b (Arrow (Tvar a) (Tvar x))
  let nctx = applyctx sigma (Map.insert 1 (sigma $ Tvar a) (incrementindices ctx))
  tau   <- typeinfer vars nctx p (sigma $ Tvar x)
  return (tau . sigma)



typeinference :: Exp -> Maybe Type
typeinference e = typeinfer variables emptyctx e (Tvar 0) <*> pure (Tvar 0)

typevariableNames :: [String]
typevariableNames = concatMap (`replicateM` ['A'..'Z']) [1..]

variables :: [Variable]
variables = [1..]

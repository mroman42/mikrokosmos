module Types
  ( Type (Tvar, Arrow)
  , typeinfer
  )
where

import Lambda

import qualified Data.Map as Map

type Context      = Map.Map Integer Type
type Variable     = Integer
type Substitution = Type -> Type
data Type         = Tvar Variable | Arrow Type Type

instance Show Type where
  show (Tvar t)     = show t
  show (Arrow a b) = show a ++ " -> " ++ show b

subs :: Variable -> Type -> Substitution
subs x typ (Tvar y)
  | x == y    = typ
  | otherwise = Tvar y
subs x typ (Arrow a b) = Arrow (subs x typ a) (subs x typ b)

occurs :: Variable -> Type -> Bool
occurs x (Tvar y)     = x == y
occurs x (Arrow a b) = occurs x a || occurs x b

unify :: Type -> Type -> Maybe Substitution
unify (Tvar x) (Tvar y)
  | x == y    = Just id
  | otherwise = Nothing
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


incrementvars :: Type -> Type
incrementvars (Tvar n)    = Tvar (succ n)
incrementvars (Arrow a b) = Arrow (incrementvars a) (incrementvars b)


typeinfer :: Context -> Exp -> Type -> Maybe Substitution
typeinfer ctx (App p q) b = do
  sigma <- typeinfer ctx               p (Arrow (Tvar 1) (incrementvars b))
  tau   <- typeinfer (applyctx sigma ctx) q (sigma (Tvar 1))
  return (tau . sigma)
typeinfer ctx (Lambda p) b = do
  sigma <- unify b (Arrow (Tvar 1) (Tvar 1))
  tau <- typeinfer (Map.insert 1 (Tvar 1) (applyctx sigma ctx)) p (sigma (Tvar 1))
  return (tau . sigma)
typeinfer ctx (Var x) b
  | Map.member x ctx = do
      var <- Map.lookup x ctx
      unify var b
  | otherwise  = Nothing

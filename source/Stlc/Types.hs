module Stlc.Types
  ( Type (Tvar, Arrow, Times, Union, Unitty, Bottom)
  , typeinference
  , typeinfer
  , unify
  , applyctx
  , emptyctx
  , incrementindices
  , variables
  , normalizeTemplate
  , applynormalization
  , Top (Top)
  , Context
  , Variable
  , Substitution
  )
where

import Control.Monad
import Lambda

import qualified Data.Map as Map

-- | A type context is a map from deBruijn indices to types. Given
-- any lambda variable as a deBruijn index, it returns its type.
type Context = Map.Map Integer Type

-- | A type variable is an integer.
type Variable = Integer

-- | A type substitution is a function that can be applied to any type
-- to get a new one.
type Substitution = Type -> Type

-- | A type template is a free type variable or an arrow between two
-- types; that is, the function type.
data Type = Tvar Variable
          | Arrow Type Type
          | Times Type Type
          | Union Type Type
          | Unitty
          | Bottom
          deriving (Eq)

instance Show Type where
  show (Tvar t) = typevariableNames !! fromInteger t
  show (Arrow a b) = showparens a ++ " → " ++ show b
  show (Times a b) = showparens a ++ " × " ++ showparens b
  show (Union a b) = showparens a ++ " + " ++ showparens b
  show Unitty = "⊤"
  show Bottom = "⊥"

showparens :: Type -> String
showparens (Tvar t) = show (Tvar t)
showparens Unitty = show Unitty
showparens Bottom = show Bottom
showparens m = "(" ++ show m ++ ")"
  
-- | Creates the substitution given by the change of a variable for
-- the given type.
subs :: Variable -> Type -> Substitution
subs x typ (Tvar y)
  | x == y    = typ
  | otherwise = Tvar y
subs x typ (Arrow a b) = Arrow (subs x typ a) (subs x typ b)
subs x typ (Times a b) = Times (subs x typ a) (subs x typ b)
subs x typ (Union a b) = Union (subs x typ a) (subs x typ b)
subs _ _ Unitty = Unitty
subs _ _ Bottom = Bottom

-- | Returns true if the given variable appears on the type.
occurs :: Variable -> Type -> Bool
occurs x (Tvar y)    = x == y
occurs x (Arrow a b) = occurs x a || occurs x b
occurs x (Times a b) = occurs x a || occurs x b
occurs x (Union a b) = occurs x a || occurs x b
occurs _ Unitty = False
occurs _ Bottom = False

-- | Unifies two types with their most general unifier. Returns the substitution
-- that transforms any of the types into the unifier.
unify :: Type -> Type -> Maybe Substitution
unify (Tvar x) (Tvar y)
  | x == y = Just id
  | otherwise = Just (subs x (Tvar y))
unify (Tvar x) b
  | occurs x b = Nothing
  | otherwise = Just (subs x b)
unify a (Tvar y)
  | occurs y a = Nothing
  | otherwise = Just (subs y a)
unify (Arrow a b) (Arrow c d) = unifypair (a,b) (c,d)
unify (Times a b) (Times c d) = unifypair (a,b) (c,d)
unify (Union a b) (Union c d) = unifypair (a, b) (c, d)
unify Unitty Unitty = Just id
unify Bottom Bottom = Just id
unify _ _ = Nothing

-- | Unifies a pair of types
unifypair :: (Type,Type) -> (Type,Type) -> Maybe Substitution
unifypair (a,b) (c,d) = do
  p <- unify b d
  q <- unify (p a) (p c)
  return (q . p)

-- | Apply a substitution to all the types on a type context.
applyctx :: Substitution -> Context -> Context
applyctx = Map.map

-- | The empty context.
emptyctx :: Context
emptyctx = Map.empty

-- | Increments all the indices of a given context. It is useful for
-- adapting the context to a new scope.
incrementindices :: Context -> Context
incrementindices = Map.mapKeys succ

-- | Type inference algorithm. Infers a type from a given context and expression
-- with a set of constraints represented by a unifier type. The result type must
-- be unifiable with this given type.
typeinfer :: [Variable] -- ^ List of fresh variables
          -> Context    -- ^ Type context
          -> Exp        -- ^ Lambda expression whose type has to be inferred
          -> Type       -- ^ Constraint
          -> Maybe Substitution
          
typeinfer []  _ _ _ = Nothing
typeinfer [_] _ _ _ = Nothing

typeinfer _ ctx (Var n) b
   | Map.member n ctx = do
       var <- Map.lookup n ctx
       unify var b
   | otherwise  = Nothing

typeinfer (x:vars) ctx (App p q) b = do
  sigma <- typeinfer (evens vars) ctx                  p (Arrow (Tvar x) b)
  tau   <- typeinfer (odds  vars) (applyctx sigma ctx) q (sigma (Tvar x))
  return (tau . sigma)
  where
    odds [] = []
    odds [_] = []
    odds (_:e:xs) = e : odds xs
    evens [] = []
    evens [e] = [e]
    evens (e:_:xs) = e : evens xs


typeinfer (a:x:vars) ctx (Lambda p) b = do
  sigma <- unify b (Arrow (Tvar a) (Tvar x))
  let nctx = applyctx sigma (Map.insert 1 (sigma $ Tvar a) (incrementindices ctx))
  tau   <- typeinfer vars nctx p (sigma $ Tvar x)
  return (tau . sigma)

typeinfer (x:y:vars) ctx (Pair m n) a = do
  sigma <- unify a (Times (Tvar x) (Tvar y))
  tau   <- typeinfer (evens vars) (applyctx sigma         ctx) m (sigma (Tvar x))
  rho   <- typeinfer (odds  vars) (applyctx (tau . sigma) ctx) n (tau (sigma (Tvar y)))
  return (rho . tau . sigma)
  where
    odds [] = []
    odds [_] = []
    odds (_:e:xs) = e : odds xs
    evens [] = []
    evens [e] = [e]
    evens (e:_:xs) = e : evens xs


typeinfer (y:vars) ctx (Pi1 m) a = typeinfer vars ctx m (Times a (Tvar y))
typeinfer (x:vars) ctx (Pi2 m) b = typeinfer vars ctx m (Times (Tvar x) b)

typeinfer (x:y:vars) ctx (Inl m) a = do
  sigma <- unify a (Union (Tvar x) (Tvar y))
  tau <- typeinfer vars (applyctx sigma ctx) m (sigma (Tvar x))
  return (tau . sigma)

typeinfer (x:y:vars) ctx (Inr m) a = do
  sigma <- unify a (Union (Tvar x) (Tvar y))
  tau <- typeinfer vars (applyctx sigma ctx) m (sigma (Tvar y))
  return (tau . sigma)

typeinfer (x:y:vars) ctx (Caseof m f g) a = do
  sigma <- typeinfer (third1 vars) ctx                          f (Arrow (Tvar x) a)
  tau   <- typeinfer (third2 vars) (applyctx sigma ctx)         g (Arrow (sigma $ Tvar y) (sigma a))
  rho   <- typeinfer (third3 vars) (applyctx (tau . sigma) ctx) m (Union (tau . sigma $ Tvar x) (tau . sigma $ Tvar y))
  return (rho . tau . sigma)
  where
    third1 [] = []
    third1 [_] = []
    third1 [_,_] = []
    third1 (_:_:e:xs) = e : third1 xs
    third2 [] = []
    third2 [_] = []
    third2 [_,e] = [e]
    third2 (_:e:_:xs) = e : third2 xs
    third3 [] = []
    third3 [e] = [e]
    third3 [e,_] = [e]
    third3 (e:_:_:xs) = e : third3 xs

typeinfer _ _ Unit a = unify Unitty a

typeinfer vars ctx (Abort m) _ = typeinfer vars ctx m Bottom

typeinfer vars ctx (Absurd m) a = do
  sigma <- unify Bottom a
  tau   <- typeinfer vars (applyctx sigma ctx) m Bottom
  return (tau . sigma)
  

-- | Type inference of a lambda expression.
typeinference :: Exp -> Maybe Type
typeinference e = normalize <$> (typeinfer variables emptyctx e (Tvar 0) <*> pure (Tvar 0))

-- | List of possible variable names.
typevariableNames :: [String]
typevariableNames = concatMap (`replicateM` ['A'..'Z']) [1..]

-- | Infinite list of variables.
variables :: [Variable]
variables = [1..]


-- | Substitutes a set of type variables on a type template for the smaller
-- possible ones.
normalizeTemplate :: Map.Map Integer Integer -> Integer -> Type -> (Map.Map Integer Integer, Integer)
normalizeTemplate sub n (Tvar m) = case Map.lookup m sub of
                                    Just _  -> (sub, n)
                                    Nothing -> (Map.insert m n sub, succ n)
normalizeTemplate sub n (Arrow a b) =
  let (nsub, nn) = normalizeTemplate sub n a in normalizeTemplate nsub nn b
normalizeTemplate sub n (Times a b) =
  let (nsub, nn) = normalizeTemplate sub n a in normalizeTemplate nsub nn b
normalizeTemplate sub n (Union a b) =
  let (nsub, nn) = normalizeTemplate sub n a in normalizeTemplate nsub nn b
normalizeTemplate sub n Unitty = (sub, n)
normalizeTemplate sub n Bottom = (sub, n)

-- | Applies a set of variable substitutions to a type to normalize it.
applynormalization :: Map.Map Integer Integer -> Type -> Type
applynormalization sub (Tvar m) = case Map.lookup m sub of
                                    Just n -> Tvar n
                                    Nothing -> Tvar m
applynormalization sub (Arrow a b) = Arrow (applynormalization sub a) (applynormalization sub b)
applynormalization sub (Times a b) = Times (applynormalization sub a) (applynormalization sub b)
applynormalization sub (Union a b) = Union (applynormalization sub a) (applynormalization sub b)
applynormalization _ Unitty = Unitty
applynormalization _ Bottom = Bottom



-- | Normalizes a type, that is, substitutes the set of type variables for
-- the smaller possible ones.
normalize :: Type -> Type
normalize t = applynormalization (fst $ normalizeTemplate Map.empty 0 t) t




-- This is definitely not an easter egg
newtype Top = Top Type
instance Show Top where
  show (Top (Tvar t))         = typevariableNames !! fromInteger t
  show (Top (Arrow a Bottom)) = "(Ω∖" ++ showparensT (Top a)  ++ ")ᴼ"
  show (Top (Arrow a b))      = "((Ω∖" ++ showparensT (Top a) ++ ") ∪ " ++ showparensT (Top b) ++ ")ᴼ"
  show (Top (Times a b))      = showparensT (Top a) ++ " ∩ " ++ showparensT (Top b)
  show (Top (Union a b))      = showparensT (Top a) ++ " ∪ " ++ showparensT (Top b)
  show (Top Unitty)           = "Ω"
  show (Top Bottom)           = "Ø"
showparensT :: Top -> String
showparensT (Top (Tvar t)) = show (Top (Tvar t))
showparensT (Top Unitty) = show (Top Unitty)
showparensT (Top Bottom) = show (Top Bottom)
showparensT (Top m) = "(" ++ show (Top m) ++ ")"

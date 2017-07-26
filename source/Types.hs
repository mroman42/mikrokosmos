module Types
  ( Type (Tvar, Arrow)
  , typeinfer
  , typeinference
  , normalize
  )
where

import Control.Monad
import Lambda

import qualified Data.Map as Map

-- | A type context is a map from deBruijn indices to types. Given
-- any lambda variable as a deBruijn index, it returns its type.
type Context      = Map.Map Integer Type

-- | A type variable is an integer.
type Variable     = Integer

-- | A type substitution is a function that can be applied to any type
-- to get a new one.
type Substitution = Type -> Type

-- | A type template is a free type variable or an arrow between two
-- types; that is, the function type.
data Type         = Tvar Variable | Arrow Type Type
  deriving (Eq)

instance Show Type where
  show (Tvar t)                  = typevariableNames !! (fromInteger t)
  show (Arrow (Tvar x) (Tvar y)) = show (Tvar x) ++ " -> "  ++ show (Tvar y)
  show (Arrow (Tvar x) b       ) = show (Tvar x) ++ " -> "  ++ show b
  show (Arrow a        (Tvar y)) = "(" ++ show a ++ ") -> " ++ show (Tvar y)
  show (Arrow a        b       ) = "(" ++ show a ++ ") -> " ++ show b


-- | Creates the substitution given by the change of a variable for
-- the given type.
subs :: Variable -> Type -> Substitution
subs x typ (Tvar y)
  | x == y    = typ
  | otherwise = Tvar y
subs x typ (Arrow a b) = Arrow (subs x typ a) (subs x typ b)

-- | Returns true if the given variable appears on the type.
occurs :: Variable -> Type -> Bool
occurs x (Tvar y)    = x == y
occurs x (Arrow a b) = occurs x a || occurs x b

-- | Unifies two types with their most general unifier. Returns the substitution
-- that transforms any of the types into the unifier.
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
          
typeinfer [] _ _ _ = Nothing
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
    evens [a] = [a]
    evens (e:_:xs) = e : evens xs

typeinfer (a:x:vars) ctx (Lambda p) b = do
  sigma <- unify b (Arrow (Tvar a) (Tvar x))
  let nctx = applyctx sigma (Map.insert 1 (sigma $ Tvar a) (incrementindices ctx))
  tau   <- typeinfer vars nctx p (sigma $ Tvar x)
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

-- | Applies a set of variable substitutions to a type to normalize it.
applynormalization :: Map.Map Integer Integer -> Type -> Type
applynormalization sub (Tvar m) = case Map.lookup m sub of
                                    Just n -> (Tvar n)
                                    Nothing -> (Tvar m)
applynormalization sub (Arrow a b) = Arrow (applynormalization sub a) (applynormalization sub b)

-- | Normalizes a type, that is, substitutes the set of type variables for
-- the smaller possible ones.
normalize :: Type -> Type
normalize t = applynormalization (fst $ normalizeTemplate Map.empty 0 t) t

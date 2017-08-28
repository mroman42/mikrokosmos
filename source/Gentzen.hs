module Gentzen
where

import Types
import Lambda
import NamedLambda
import Data.Bifunctor
import qualified Data.Map as Map

type Matrix a = [[a]]
type CharMatrix = Matrix Char

height :: Matrix a -> Int
height = length
width :: [[a]] -> Int
width []     = 0
width (x:_) = length x

joinMatrices :: a -> Matrix a -> Matrix a -> Matrix a
joinMatrices space u v = zipWith (++) us vs
  where
    maxheight = max (height u) (height v)
    us = replicate (maxheight - height u) (replicate (width u) space) ++ u
    vs = replicate (maxheight - height v) (replicate (width v) space) ++ v

(<::>) :: CharMatrix -> CharMatrix -> CharMatrix
(<::>) = joinMatrices ' '

hsepChar :: Char
hsepChar = '─'

stackMatrices :: String -> CharMatrix -> CharMatrix -> CharMatrix
stackMatrices label u v = ut ++ [hline] ++ vt
  where
    mw = max (width u) (width v)
    us = centerMatrix mw u
    vs = centerMatrix mw v
    hline = replicate mw hsepChar ++ label
    ut = us <::> [replicate (length label) ' ']
    vt = vs <::> [replicate (length label) ' ']


centerMatrix :: Int -> CharMatrix -> CharMatrix
centerMatrix n = map (centerRow n)

centerRow :: Int -> String -> String
centerRow n s = centered
  where
    w = length s
    diff = n - w
    semicentered = replicate (diff `div` 2) ' ' ++ s
    centered = take n (semicentered ++ repeat ' ')

showMatrixChar :: CharMatrix -> String
showMatrixChar = unlines

textMatrix :: String -> CharMatrix
textMatrix s = [centerRow (length s + 2) s]

deductionMatrix :: CharMatrix -> String -> [CharMatrix] -> CharMatrix
deductionMatrix inference label blocks = stackMatrices label top inference
  where
    top = foldr (<::>) [] blocks

data ProofTree a l = Inference a | Deduction a l [ProofTree a l]
instance Bifunctor ProofTree where
  bimap f _ (Inference a)      = Inference (f a)
  bimap f g (Deduction a l ps) = Deduction (f a) (g l) (map (bimap f g) ps)

matrixProofTree :: ProofTree String String -> CharMatrix
matrixProofTree (Inference x)      = textMatrix x
matrixProofTree (Deduction x l xs) = deductionMatrix (textMatrix x) l (map matrixProofTree xs)

showProofTree :: ProofTree String String -> String
showProofTree = showMatrixChar . matrixProofTree


data Label = Lponens | Labs | Lpair | Lpi1 | Lpi2 | Linr | Linl | Lcase | Lunit | Labort | Labsurd
instance Show Label where
  show Lponens = "(impl)"
  show Labs = "(abs)"
  show Lpair = "(pair)"
  show Lpi1 = "(pi1)"
  show Lpi2 = "(pi2)"
  show Linr = "(inr)"
  show Linl = "(inl)"
  show Lcase = "(case)"
  show Lunit = "(unit)"
  show Labort = "(abort)"
  show Labsurd = "(absurd)"
  
type TermDiagram = (Exp,Type)
type Depth = Int

gentzendiagram :: Exp -> Maybe (ProofTree String String)
gentzendiagram l = do
  (_, d) <- typeinfer' variables emptyctx l (Tvar 0)
  maintype <- typeinference l
  let normtemplate = normalizeTemplate Map.empty 0 maintype
  return (bimap showTermDiagram show d)
  where
    showTermDiagram (expl, t) = show (nameExp expl) ++ "::" ++ show t

-- | Type inference algorithm. Infers a type from a given context and expression
-- with a set of constraints represented by a unifier type. The result type must
-- be unifiable with this given type.
-- Generalized version.
typeinfer' :: [Variable] -- ^ List of fresh variables
          -> Context    -- ^ Type context
          -> Exp        -- ^ Lambda expression whose type has to be inferred
          -> Type       -- ^ Constraint
          -> Maybe (Substitution, ProofTree TermDiagram Label)
          
typeinfer' []  _ _ _ = Nothing
typeinfer' [_] _ _ _ = Nothing

typeinfer' _ ctx l@(Var n) b
   | Map.member n ctx = do
       var <- Map.lookup n ctx
       ss <- unify var b
       return (ss, Inference (l, ss b))
   | otherwise  = Nothing

typeinfer' (x:vars) ctx l@(App p q) b = do
  (sigma, d1) <- typeinfer' (evens vars) ctx                  p (Arrow (Tvar x) b)
  (tau,   d2) <- typeinfer' (odds  vars) (applyctx sigma ctx) q (sigma (Tvar x))
  let ss = tau . sigma
  return (ss, Deduction (l, ss b) Lponens [d1,d2])
  where
    odds [] = []
    odds [_] = []
    odds (_:e:xs) = e : odds xs
    evens [] = []
    evens [e] = [e]
    evens (e:_:xs) = e : evens xs


typeinfer' (a:x:vars) ctx l@(Lambda p) b = do
  sigma <- unify b (Arrow (Tvar a) (Tvar x))
  let nctx = applyctx sigma (Map.insert 1 (sigma $ Tvar a) (incrementindices ctx))
  (tau,   d2) <- typeinfer' vars nctx p (sigma $ Tvar x)
  let ss = tau . sigma
  return (ss, Deduction (l, ss b) Labs [d2])

typeinfer' (x:y:vars) ctx l@(Pair m n) a = do
  sigma <- unify a (Times (Tvar x) (Tvar y))
  (tau, d1) <- typeinfer' (evens vars) (applyctx sigma         ctx) m (sigma (Tvar x))
  (rho, d2) <- typeinfer' (odds  vars) (applyctx (tau . sigma) ctx) n (tau (sigma (Tvar y)))
  let ss = rho . tau . sigma
  return (ss, Deduction (l, ss a) Lpair [d1,d2])
  where
    odds [] = []
    odds [_] = []
    odds (_:e:xs) = e : odds xs
    evens [] = []
    evens [e] = [e]
    evens (e:_:xs) = e : evens xs


typeinfer' (y:vars) ctx l@(Pi1 m) a = do
  (sigma, d1) <- typeinfer' vars ctx m (Times a (Tvar y))
  let ss = sigma
  return (ss, Deduction (l, ss a) Lpi1 [d1])
  
typeinfer' (x:vars) ctx l@(Pi2 m) b = do
  (sigma, d1) <- typeinfer' vars ctx m (Times (Tvar x) b)
  let ss = sigma
  return (ss, Deduction (l, ss b) Lpi2 [d1])

typeinfer' (x:y:vars) ctx l@(Inl m) a = do
  sigma <- unify a (Union (Tvar x) (Tvar y))
  (tau, d1) <- typeinfer' vars (applyctx sigma ctx) m (sigma (Tvar x))
  let ss = tau . sigma
  return (ss, Deduction (l, ss a) Linl [d1])

typeinfer' (x:y:vars) ctx l@(Inr m) a = do
  sigma <- unify a (Union (Tvar x) (Tvar y))
  (tau, d1) <- typeinfer' vars (applyctx sigma ctx) m (sigma (Tvar y))
  let ss = tau . sigma
  return (ss, Deduction (l, ss a) Linr [d1])

typeinfer' (x:y:vars) ctx l@(Caseof m f g) a = do
  (sigma, d1) <- typeinfer' (third1 vars) ctx                          f (Arrow (Tvar x) a)
  (tau,   d2) <- typeinfer' (third2 vars) (applyctx sigma ctx)         g (Arrow (sigma $ Tvar y) (sigma a))
  (rho,   d3) <- typeinfer' (third3 vars) (applyctx (tau . sigma) ctx) m (Union (tau . sigma $ Tvar x) (tau . sigma $ Tvar y))
  let ss = rho . tau . sigma 
  return (ss, Deduction (l, ss a) Lcase [d1,d2,d3])
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

typeinfer' _ _ l@Unit a = do
  ss <- unify Unitty a
  return (ss, Deduction (l, ss a) Lunit [])

typeinfer' vars ctx l@(Abort m) a = do
  (ss, d1) <- typeinfer' vars ctx m Bottom
  return (ss, Deduction (l, ss a) Labort [d1])

typeinfer' vars ctx l@(Absurd m) a = do
  sigma     <- unify Bottom a
  (tau, d1) <- typeinfer' vars (applyctx sigma ctx) m Bottom
  let ss = tau . sigma
  return (ss, Deduction (l, ss a) Labsurd [d1])

                     

module MultiBimap where

-- Based in the bimap package
-- https://hackage.haskell.org/package/bimap-0.3.2/docs/Data-Bimap.html

import qualified Data.Map      as M
import qualified Data.MultiMap as MM

data MultiBimap a b = MkMultiBimap (M.Map a b) (MM.MultiMap b a)

-- | The empty multi-bimap
empty :: MultiBimap a b
empty = MkMultiBimap M.empty MM.empty

-- | True if the multi-bimap is empty
null :: MultiBimap a b -> Bool
null (MkMultiBimap left _) = M.null left

-- | Inserts a key-value in the multibimap.
-- The value can have been used earlier.
insert :: (Ord a, Ord b) => a -> b -> MultiBimap a b -> MultiBimap a b
insert x y (MkMultiBimap left right) =
  MkMultiBimap (M.insert x y left) (MM.insert y x right)

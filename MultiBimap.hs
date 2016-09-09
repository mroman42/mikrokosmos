module MultiBimap
  ( MultiBimap
  , empty
  , null
  , insert
  , lookup
  , lookupR
  )
where

-- This is a package which allows me to abstract a bidirectional multimap without
-- having to worry about implementation details. It is useful in the translation
-- between lambda expressions and names.
-- 
-- Based on the bimap package:
-- https://hackage.haskell.org/package/bimap-0.3.2/docs/Data-Bimap.html

import qualified Data.Map      as M
import qualified Data.MultiMap as MM
import           Prelude       hiding (null,lookup)

data MultiBimap k v = MkMultiBimap (MM.MultiMap k v) (M.Map v k)

-- | The empty multi-bimap
empty :: MultiBimap k v
empty = MkMultiBimap MM.empty M.empty

-- | True if the multi-bimap is empty
null :: MultiBimap k v -> Bool
null (MkMultiBimap _ right) = M.null right

-- | Inserts a key-value in the multi-bimap.
-- The value can have been used earlier.
insert :: (Ord k, Ord v) => k -> v -> MultiBimap k v -> MultiBimap k v
insert k v (MkMultiBimap left right) =
  MkMultiBimap (MM.insert k v left) (M.insert v k right)

-- | Lookup a key in the multi-bimap, returning the list of
-- associated values.
lookup :: (Ord k, Ord v) => k -> MultiBimap k v -> [v]
lookup k (MkMultiBimap left _) = MM.lookup k left

-- | Lookup a right value in the multi-bimap, returning the associated key.
lookupR :: (Ord k, Ord v) => v -> MultiBimap k v -> Maybe k
lookupR v (MkMultiBimap _ right) = M.lookup v right

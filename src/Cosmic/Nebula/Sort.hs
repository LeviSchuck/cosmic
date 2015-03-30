{-# LANGUAGE
    NoImplicitPrelude
  #-}
{-|
This has convenience function for sorting facts within
"Cosmic.Nebula.Facts"
-}
module Cosmic.Nebula.Sort where

import Data.Ord
import Prelude(Eq(..),otherwise)

import Cosmic.Dust.Value
import Cosmic.Dust.Fact

import qualified Data.Vector as V

-- | Sort kinds, with both generic, and value specific
-- comparison functinos
data FactSort
  = ByEntity
  | ByAttribute
  | ByPreValue
  | ByValue
  | ByMetaValue
  | FnPre   (ParticleKind -> ParticleKind -> Ordering)
  -- ^ Comparison function for PreValues
  | FnValue (ParticleKind -> ParticleKind -> Ordering)
  -- ^ Comparison function for the main Values
  | FnMeta  (ParticleKind -> ParticleKind -> Ordering)
  -- ^ Comparison functino for the meta values

-- | A list of things to sort in order by.
-- 
-- If the first kind is already sorted by, then we will
-- try to sort by the next sort kind.
type SortList = V.Vector FactSort

-- | A function to be used in 'Cosmic.Nebula.Facts' for sorting
-- convenience.
bySortList  :: (Ord p, Ord e, Ord a)
                => SortList
                -- ^ List of ways to sort by in order
                -> Fact p e a
                -- ^ First Fact
                -> Fact p e a
                -- ^ Second Fact
                -> Ordering
                -- ^ Resulting ordering
bySortList sl f1 f2 = V.foldl sc EQ sl
  where
    sc c k
      | c   /= EQ = c
      | otherwise = case k of
        ByEntity    -> fEntity    f1 `compare` fEntity    f2
        ByAttribute -> fAttribute f1 `compare` fAttribute f2
        ByPreValue  -> fPreVal    f1 `compare` fPreVal    f2
        ByValue     -> fValue     f1 `compare` fValue     f2
        ByMetaValue -> fMetaVal   f1 `compare` fMetaVal   f2
        FnPre   cmp -> fPreVal    f1   `cmp`   fPreVal    f2
        FnValue cmp -> fValue     f1   `cmp`   fValue     f2
        FnMeta  cmp -> fMetaVal   f1   `cmp`   fMetaVal   f2

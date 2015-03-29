{-# LANGUAGE NoImplicitPrelude #-}
module Cosmic.Nebula.Sort where

import Data.Ord
import Prelude(Eq(..),Show(..),otherwise)

import Cosmic.Dust.Fact

import qualified Data.Vector as V

data FactSort
  = ByEntity
  | ByAttribute
  | ByPreValue
  | ByValue
  | ByMetaValue
  deriving(Eq,Ord,Show)

type SortList = V.Vector FactSort

bySortList  :: (Ord p, Ord e, Ord a)
                => SortList
                -> Fact p e a
                -> Fact p e a
                -> Ordering
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

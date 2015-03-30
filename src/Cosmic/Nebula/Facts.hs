{-# LANGUAGE
    NoImplicitPrelude
  #-}
{-|
A basic "Data.Vector" of 'Fact's that can also be sorted however
you please.
-}
module Cosmic.Nebula.Facts where

import Control.Monad.ST
import Data.Ord

import Prelude(($),(/=),Eq(..),otherwise)

import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VS

import Cosmic.Dust.Fact

-- | A basic wrapper for a collection of facts
type Facts px ent attr = V.Vector (Fact px ent attr)

-- | Allows us to sort facts, however partitions are always
-- respected, no matter what is chosen.
sortFacts :: (Ord p, Ord e, Ord a)
          => (Fact p e a -> Fact p e a -> Ordering)
          -- ^ Comparison function to sort by
          -> Facts p e a
          -- ^ Input unsorted facts
          -> Facts p e a
          -- ^ Output sorted facts
sortFacts s f = runST $ do
  fs <- V.thaw f
  VS.sortBy sf fs
  V.freeze fs
  where
    sf f1 f2
      | p1  /= p2 = p1 `compare` p2
      | otherwise = s f1 f2
      where
        p1 = fPartition f1
        p2 = fPartition f2

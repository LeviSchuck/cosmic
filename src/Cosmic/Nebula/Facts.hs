{-# LANGUAGE NoImplicitPrelude #-}
module Cosmic.Nebula.Facts where

import Control.Monad.ST
import Data.Ord

import Prelude(($),(/=),Eq(..),otherwise)

import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VS

import Cosmic.Dust.Fact

type Facts px ent attr = V.Vector (Fact px ent attr)

sortFacts :: (Ord p, Ord e, Ord a)
          => (Fact p e a -> Fact p e a -> Ordering)
          -> Facts p e a
          -> Facts p e a
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

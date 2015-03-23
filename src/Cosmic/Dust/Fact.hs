{-# LANGUAGE NoImplicitPrelude #-}
module Cosmic.Dust.Fact where

-- Type classes from Base
import Prelude(Eq(..),Show(..))
import Data.Ord

-- Internal 
import Cosmic.Dust.Value
import Cosmic.Dust.Identifiers

data Fact partition
  = Fact
  { fPartition  :: partition
  , fEntity     :: EntityID
  , fAttribute  :: AttributeID
  , fPreVal     :: ParticleKind
  , fVal        :: ParticleKind
  , fPostVal    :: ParticleKind
  } deriving(Eq,Ord,Show)

type StorageFact = Fact StoragePartitionID
type WireFact = Fact WirePartitionID

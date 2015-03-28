{-# LANGUAGE NoImplicitPrelude #-}
module Cosmic.Dust.Fact where

-- Type classes from Base
import Prelude(Eq(..),Show(..))
import Data.Ord

-- Internal 
import Cosmic.Dust.Value
import Cosmic.Dust.Identifiers

data Fact px ent attr
  = Fact
  { fPartition  :: px
  , fEntity     :: ent
  , fAttribute  :: attr
  , fPreVal     :: ParticleKind
  , fVal        :: ParticleKind
  , fMetaVal    :: ParticleKind
  } deriving(Eq,Ord,Show)

type StorageFact
  = Fact
    StoragePartitionID
    StorageEntityID
    StorageAttributeID

type WireFact
  = Fact
    WirePartitionID
    WireEntityID
    WireAttributeID

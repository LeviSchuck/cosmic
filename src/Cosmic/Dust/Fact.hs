{-# LANGUAGE
    NoImplicitPrelude
  , DeriveDataTypeable
  #-}
module Cosmic.Dust.Fact where

-- Type classes from Base
import Prelude(Eq(..),Show(..))
import Data.Ord
import Data.Typeable
import Data.Data

-- Internal 
import Cosmic.Dust.Value
import Cosmic.Dust.Identifiers

data Fact px ent attr
  = Fact
  { fPartition  :: px
  , fEntity     :: ent
  , fAttribute  :: attr
  , fPreVal     :: ParticleKind
  , fValue      :: ParticleKind
  , fMetaVal    :: ParticleKind
  } deriving(Eq,Ord,Show,Typeable,Data)

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

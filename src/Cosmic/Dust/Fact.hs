{-# LANGUAGE
    NoImplicitPrelude
  , DeriveDataTypeable
  #-}
{-|

A 'Fact' is a value with several descriptors about the value.

A partition can be used to split different kinds of values.

Entities group values together, which are specialized by attribute.

PreValues can be used in conjunction with attributes to form
collections of values in maps and sets.

It is up to your implementation on top of 'Fact's to respect
sets vs maps.

Meta values are more for just 'ContextualParticle's to detail
information like 'Cosmic.Dust.Identifiers.TransactionID' and
'Cosmic.Dust.Identifiers.Assertion'

-}
module Cosmic.Dust.Fact where

-- Type classes from Base
import Prelude(Eq(..),Show(..))
import Data.Ord
import Data.Typeable
import Data.Data

-- Internal 
import Cosmic.Dust.Value
import Cosmic.Dust.Identifiers

-- | A datatype to hold data to constitute a fact, as described
-- in the top of this module.
data Fact px ent attr
  = Fact
  { fPartition  :: px
  , fEntity     :: ent
  , fAttribute  :: attr
  , fPreVal     :: ParticleKind
  , fValue      :: ParticleKind
  , fMetaVal    :: ParticleKind
  } deriving(Eq,Ord,Show,Typeable,Data)


-- | Convenience type for stored facts
type StorageFact
  = Fact
    StoragePartitionID
    StorageEntityID
    StorageAttributeID

-- | Convenience type for wired facts
type WireFact
  = Fact
    WirePartitionID
    WireEntityID
    WireAttributeID

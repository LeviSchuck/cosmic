{-# LANGUAGE NoImplicitPrelude #-}
module Cosmic.Dust.Identifiers
  ( TransactionID(..)
  , StoragePartitionID(..)
  , StorageAttributeID(..)
  , StorageEntityID(..)
  , WirePartitionID(..)
  , WireAttributeID(..)
  , WireEntityID(..)
  ) where

import Prelude(Eq(..),Show(..),return,String,($),(++),undefined)
import Data.Either
import Data.Maybe
import Data.Word
import Data.Ord

import Cosmic.Dust.Extract
import Cosmic.Dust.Value
import Cosmic.Dust.Utils

newtype StoragePartitionID
  = StoragePartitionID
  { unSPx :: Word32
  } deriving(Eq,Ord,Show)

newtype StorageAttributeID
  = StorageAttributeID
  { unSAttr :: Word32
  } deriving(Eq,Ord,Show)

newtype StorageEntityID
  = StorageEntityID
  { unSEnt :: Word64
  } deriving(Eq,Ord,Show)

newtype WirePartitionID
  = WirePartitionID
  { unWPx :: Word8
  } deriving(Eq,Ord,Show)

newtype WireAttributeID
  = WireAttributeID
  { unWAttr :: Word32
  } deriving(Eq,Ord,Show)

newtype WireEntityID
  = WireEntityID
  { unWEnt :: Word64
  } deriving(Eq,Ord,Show)

newtype TransactionID
  = TransactionID
  { unTx :: Word64
  } deriving(Eq,Ord,Show)

data Assertion
  = Assert
  | Redact
  deriving(Eq,Ord,Show)

instance EnumWord8 Assertion where
  toWord8 Assert = 1
  toWord8 Redact = 2
  fromWord8 1 = Just Assert
  fromWord8 2 = Just Redact
  fromWord8 _ = Nothing

instance TaggedDbType8 StoragePartitionID where
  getTag _ = 129
instance TaggedDbType8 WirePartitionID where
  getTag _ = 130

instance TaggedDbType8 StorageAttributeID where
  getTag _ = 132
instance TaggedDbType8 WireAttributeID where
  getTag _ = 133

instance TaggedDbType8 StorageEntityID where
  getTag _ = 136
instance TaggedDbType8 WireEntityID where
  getTag _ = 137

instance TaggedDbType8 TransactionID where
  getTag _ = 144

instance TaggedDbType8 Assertion where
  getTag _ = 160

takeUnit :: PrimitiveParticle -> Either String ()
takeUnit = extractParticle

instance Extract StoragePartitionID where
  extractName _ = "Storage PartitionID"
  extractEither = extractContext1 StoragePartitionID
  -- Disallow semi-direct extraction
  extractParticle prim = badPrim "Storage PartitionID" prim

instance Extract WirePartitionID where
  extractName _ = "Wire PartitionID"
  extractEither = extractContext1 WirePartitionID
  -- Disallow semi-direct extraction
  extractParticle prim = badPrim "Wire PartitionID" prim

instance Extract StorageAttributeID where
  extractName _ = "Storage AttributeID"
  extractEither = extractContext1 StorageAttributeID
  -- Disallow semi-direct extraction
  extractParticle prim = badPrim "Storage AttributeID" prim

instance Extract WireAttributeID where
  extractName _ = "Wire AttributeID"
  extractEither = extractContext1 WireAttributeID
  -- Disallow semi-direct extraction
  extractParticle prim = badPrim "Wire AttributeID" prim

instance Extract StorageEntityID where
  extractName _ = "Storage EntityID"
  extractEither = extractContext1 StorageEntityID
  -- Disallow semi-direct extraction
  extractParticle prim = badPrim "Storage EntityID" prim

instance Extract WireEntityID where
  extractName _ = "Wire EntityID"
  extractEither = extractContext1 WireEntityID
  -- Disallow semi-direct extraction
  extractParticle prim = badPrim "Wire EntityID" prim

instance Extract TransactionID where
  extractName _ = "TransactionID"
  extractEither = extractContext1 TransactionID
  -- Disallow semi-direct extraction
  extractParticle prim = badPrim "TransactionID" prim

instance Extract Assertion where
  extractName _ = "Assertion"
  extractEither = extractTaggedEnum Assert
  -- Disallow semi-direct extraction
  extractParticle prim = badPrim "Assertion" prim

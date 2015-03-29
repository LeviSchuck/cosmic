{-# LANGUAGE
    NoImplicitPrelude
  , DeriveDataTypeable
  #-}
module Cosmic.Dust.Identifiers
  ( TransactionID(..)
  , StoragePartitionID(..)
  , StorageAttributeID(..)
  , StorageEntityID(..)
  , WirePartitionID(..)
  , WireAttributeID(..)
  , WireEntityID(..)
  ) where

import Prelude(Eq(..),Show(..))
import Data.Maybe
import Data.Word
import Data.Ord
import Data.Typeable
import Data.Data
import Control.Applicative(Const(..))


import Cosmic.Dust.Value
import Cosmic.Dust.Extract
import Cosmic.Dust.Utils

newtype StoragePartitionID
  = StoragePartitionID
  { unSPx :: Word32
  } deriving(Eq,Ord,Show,Typeable,Data)

newtype StorageAttributeID
  = StorageAttributeID
  { unSAttr :: Word32
  } deriving(Eq,Ord,Show,Typeable,Data)

newtype StorageEntityID
  = StorageEntityID
  { unSEnt :: Word64
  } deriving(Eq,Ord,Show,Typeable,Data)

newtype WirePartitionID
  = WirePartitionID
  { unWPx :: Word8
  } deriving(Eq,Ord,Show,Typeable,Data)

newtype WireAttributeID
  = WireAttributeID
  { unWAttr :: Word16
  } deriving(Eq,Ord,Show,Typeable,Data)

newtype WireEntityID
  = WireEntityID
  { unWEnt :: Word16
  } deriving(Eq,Ord,Show,Typeable,Data)

newtype TransactionID
  = TransactionID
  { unTx :: Word64
  } deriving(Eq,Ord,Show,Typeable,Data)

data Assertion
  = Assert
  | Redact
  deriving(Eq,Ord,Show,Typeable,Data)



instance EnumWord8 Assertion where
  toWord8 Assert = 1
  toWord8 Redact = 2
  fromWord8 1 = Just Assert
  fromWord8 2 = Just Redact
  fromWord8 _ = Nothing

instance TaggedDbType8 StoragePartitionID where
  getTag = Const 129
instance TaggedDbType8 WirePartitionID where
  getTag = Const 130

instance TaggedDbType8 StorageAttributeID where
  getTag = Const 132
instance TaggedDbType8 WireAttributeID where
  getTag = Const 133

instance TaggedDbType8 StorageEntityID where
  getTag = Const 136
instance TaggedDbType8 WireEntityID where
  getTag = Const 137

instance TaggedDbType8 TransactionID where
  getTag = Const 144

instance TaggedDbType8 Assertion where
  getTag = Const 160

instance Extract StoragePartitionID where
  extractNameKind = constByKind KindContext
  expectedPrim    = constByPrim PWord32
  extractNameType = constByTypeable
  extractEither   = extractContext1 StoragePartitionID
  -- Disallow semi-direct extraction
  extractParticle prim = badPrim prim

instance Extract WirePartitionID where
  extractNameKind = constByKind KindContext
  expectedPrim    = constByPrim PWord8
  extractNameType = constByTypeable
  extractEither   = extractContext1 WirePartitionID
  -- Disallow semi-direct extraction
  extractParticle prim = badPrim prim

instance Extract StorageAttributeID where
  extractNameKind = constByKind KindContext
  expectedPrim    = constByPrim PWord32
  extractNameType = constByTypeable
  extractEither   = extractContext1 StorageAttributeID
  -- Disallow semi-direct extraction
  extractParticle = badPrim

instance Extract WireAttributeID where
  extractNameKind = constByKind KindContext
  expectedPrim    = constByPrim PWord16
  extractNameType = constByTypeable
  extractEither   = extractContext1 WireAttributeID
  -- Disallow semi-direct extraction
  extractParticle prim = badPrim prim

instance Extract StorageEntityID where
  extractNameKind = constByKind KindContext
  expectedPrim    = constByPrim PWord64
  extractNameType = constByTypeable
  extractEither   = extractContext1 StorageEntityID
  -- Disallow semi-direct extraction
  extractParticle prim = badPrim prim

instance Extract WireEntityID where
  extractNameKind = constByKind KindContext
  expectedPrim    = constByPrim PWord16
  extractNameType = constByTypeable
  extractEither   = extractContext1 WireEntityID
  -- Disallow semi-direct extraction
  extractParticle prim = badPrim prim

instance Extract TransactionID where
  extractNameKind = constByKind KindContext
  expectedPrim    = constByPrim PWord64
  extractNameType = constByTypeable
  extractEither   = extractContext1 TransactionID
  -- Disallow semi-direct extraction
  extractParticle prim = badPrim prim

instance Extract Assertion where
  extractNameKind = constByKind KindSingle
  expectedPrim    = constByPrim PWord8
  extractNameType = constByTypeable
  extractEither   = extractTaggedEnum c
    where c = Const () :: Const () Assertion
  -- Disallow semi-direct extraction
  extractParticle prim = badPrim prim

{-# LANGUAGE NoImplicitPrelude #-}
module Cosmic.Dust.Identifiers
  ( TransactionID(..)
  , StoragePartitionID(..)
  , WirePartitionID(..)
  , AttributeID(..)
  , EntityID(..)
  ) where

import Prelude(Eq(..),Show(..),return,String,($))
import Data.Either
import Data.Word
import Data.Ord

import Cosmic.Dust.Extract
import Cosmic.Dust.Value

newtype TransactionID
  = TransactionID
  { unTx :: Word64
  } deriving(Eq,Ord,Show)

newtype StoragePartitionID
  = StoragePartitionID
  { unSPx :: Word32
  } deriving(Eq,Ord,Show)

newtype WirePartitionID
  = WirePartitionID
  { unWPx :: Word8
  } deriving(Eq,Ord,Show)

newtype AttributeID
  = AttributeID
  { unAttr :: Word32
  } deriving(Eq,Ord,Show)

newtype EntityID
  = EntityID
  { unEnt :: Word64
  } deriving(Eq,Ord,Show)

takeUnit :: PrimitiveParticle -> Either String ()
takeUnit = extractParticle

instance Extract TransactionID where
  extractName _ = "Transaction"
  -- Disallow semi-direct extraction
  extractParticle prim = badPrim "Transaction" prim
  -- extractEither     :: ParticleKind -> Either String a
  extractEither (KindContext (ContextualParticle u t)) = do
    _ <- takeUnit u
    tx <- extractParticle t
    return $ TransactionID tx

instance Extract StoragePartitionID where
  extractName _ = "Storage PartitionID"
  -- Disallow semi-direct extraction
  extractParticle prim = badPrim "Storage PartitionID" prim
  -- extractEither     :: ParticleKind -> Either String a
  extractEither (KindContext (ContextualParticle u t)) = do
    _ <- takeUnit u
    spx <- extractParticle t
    return $ StoragePartitionID spx

instance Extract WirePartitionID where
  extractName _ = "Wire PartitionID"
  -- Disallow semi-direct extraction
  extractParticle prim = badPrim "Wire PartitionID" prim
  -- extractEither     :: ParticleKind -> Either String a
  extractEither (KindContext (ContextualParticle u t)) = do
    _ <- takeUnit u
    wpx <- extractParticle t
    return $ WirePartitionID wpx

instance Extract AttributeID where
  extractName _ = "Attribute"
  -- Disallow semi-direct extraction
  extractParticle prim = badPrim "Attribute" prim
  -- extractEither     :: ParticleKind -> Either String a
  extractEither (KindContext (ContextualParticle u t)) = do
    _ <- takeUnit u
    attr <- extractParticle t
    return $ AttributeID attr

instance Extract EntityID where
  extractName _ = "Entity"
  -- Disallow semi-direct extraction
  extractParticle prim = badPrim "Entity" prim
  -- extractEither     :: ParticleKind -> Either String a
  extractEither (KindContext (ContextualParticle u t)) = do
    _ <- takeUnit u
    ent <- extractParticle t
    return $ EntityID ent

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Cosmic.Dust.Value where

-- Base
import Prelude(Int,Integer,Float,Double,Rational,Bool(..))
import Data.Word(Word8,Word16,Word32,Word64)
import Data.Int(Int64)

import Prelude(Eq(..),Show(..),(++))
import Data.Ord

-- Externals
import Data.Text as T
import Data.ByteString as B

-- Internals
import Cosmic.Dust.Identifiers

data MockTODO = MockTODO
  deriving(Eq,Ord,Show)

data Particle db
  = PBool     Bool
  -- Numbers
  | PInt      Int
  | PBigInt   Integer
  | PFloat    Float
  | PDouble   Double
  | PRatio    Rational
  -- Computery numbers
  | PWord8    Word8
  | PWord16   Word16
  | PWord32   Word32
  | PWord64   Word64
  | PInt64    Int64
  -- External basics
  | PText     T.Text
  | PBytes    B.ByteString
  -- Externals not-so-basics
  | PURI      MockTODO
  | PUUID     MockTODO
  -- Timey wimey things
  | PTime     MockTODO
  | PDate     MockTODO
  | PPeriod   MockTODO -- Time range
  | PInstance MockTODO
  -- Finally the other things which may be used to link to others within our crazy house
  | PPartition    (DatabaseIDs db PartitionK)
  | PEntity       (DatabaseIDs db EntityRefK)
  | PTransaction  (DatabaseIDs db TransactionK)
  | PAttribute    (DatabaseIDs db AttributeK)
  -- deriving(Eq,Ord,Show)

instance ( Eq (DatabaseIDs db PartitionK)
         , Eq (DatabaseIDs db EntityRefK)
         , Eq (DatabaseIDs db TransactionK)
         , Eq (DatabaseIDs db AttributeK)) =>
  Eq (Particle db) where
    PBool a         == PBool b          = a == b
    PInt a          == PInt b           = a == b
    PBigInt a       == PBigInt b        = a == b
    PFloat a        == PFloat b         = a == b
    PDouble a       == PDouble b        = a == b
    PRatio a        == PRatio b         = a == b
    PWord8 a        == PWord8 b         = a == b
    PWord16 a       == PWord16 b        = a == b
    PWord32 a       == PWord32 b        = a == b
    PWord64 a       == PWord64 b        = a == b
    PInt64 a        == PInt64 b         = a == b
    PText a         == PText b          = a == b
    PBytes a        == PBytes b         = a == b
    PURI a          == PURI b           = a == b
    PUUID a         == PUUID b          = a == b
    PTime a         == PTime b          = a == b
    PDate a         == PDate b          = a == b
    PPeriod a       == PPeriod b        = a == b
    PInstance a     == PInstance b      = a == b
    PPartition a    == PPartition b     = a == b
    PEntity a       == PEntity b        = a == b
    PTransaction a  == PTransaction b   = a == b
    PAttribute a    == PAttribute b     = a == b
    _ == _ = False

instance ( Ord (DatabaseIDs db PartitionK)
         , Ord (DatabaseIDs db EntityRefK)
         , Ord (DatabaseIDs db TransactionK)
         , Ord (DatabaseIDs db AttributeK)) =>
  Ord (Particle db) where
    PBool a         `compare` PBool b         = a `compare` b
    PInt a          `compare` PInt b          = a `compare` b
    PBigInt a       `compare` PBigInt b       = a `compare` b
    PFloat a        `compare` PFloat b        = a `compare` b
    PDouble a       `compare` PDouble b       = a `compare` b
    PRatio a        `compare` PRatio b        = a `compare` b
    PWord8 a        `compare` PWord8 b        = a `compare` b
    PWord16 a       `compare` PWord16 b       = a `compare` b
    PWord32 a       `compare` PWord32 b       = a `compare` b
    PWord64 a       `compare` PWord64 b       = a `compare` b
    PInt64 a        `compare` PInt64 b        = a `compare` b
    PText a         `compare` PText b         = a `compare` b
    PBytes a        `compare` PBytes b        = a `compare` b
    PURI a          `compare` PURI b          = a `compare` b
    PUUID a         `compare` PUUID b         = a `compare` b
    PTime a         `compare` PTime b         = a `compare` b
    PDate a         `compare` PDate b         = a `compare` b
    PPeriod a       `compare` PPeriod b       = a `compare` b
    PInstance a     `compare` PInstance b     = a `compare` b
    PPartition a    `compare` PPartition b    = a `compare` b
    PEntity a       `compare` PEntity b       = a `compare` b
    PTransaction a  `compare` PTransaction b  = a `compare` b
    PAttribute a    `compare` PAttribute b    = a `compare` b
    PBool         _ `compare` _ = GT
    PInt          _ `compare` _ = GT
    PBigInt       _ `compare` _ = GT
    PFloat        _ `compare` _ = GT
    PDouble       _ `compare` _ = GT
    PRatio        _ `compare` _ = GT
    PWord8        _ `compare` _ = GT
    PWord16       _ `compare` _ = GT
    PWord32       _ `compare` _ = GT
    PWord64       _ `compare` _ = GT
    PInt64        _ `compare` _ = GT
    PText         _ `compare` _ = GT
    PBytes        _ `compare` _ = GT
    PURI          _ `compare` _ = GT
    PUUID         _ `compare` _ = GT
    PTime         _ `compare` _ = GT
    PDate         _ `compare` _ = GT
    PPeriod       _ `compare` _ = GT
    PInstance     _ `compare` _ = GT
    PPartition    _ `compare` _ = GT
    PEntity       _ `compare` _ = GT
    PTransaction  _ `compare` _ = GT
    -- PAttribute    _ `compare` _ = GT
    _ `compare` _               = LT

instance ( Show (DatabaseIDs db PartitionK)
         , Show (DatabaseIDs db EntityRefK)
         , Show (DatabaseIDs db TransactionK)
         , Show (DatabaseIDs db AttributeK)) =>
  Show (Particle db) where
    show (PBool a)         = "PBool " ++ show a
    show (PInt a)          = "PInt " ++ show a
    show (PBigInt a)       = "PBigInt " ++ show a
    show (PFloat a)        = "PFloat " ++ show a
    show (PDouble a)       = "PDouble " ++ show a
    show (PRatio a)        = "PRatio " ++ show a
    show (PWord8 a)        = "PWord8 " ++ show a
    show (PWord16 a)       = "PWord16 " ++ show a
    show (PWord32 a)       = "PWord32 " ++ show a
    show (PWord64 a)       = "PWord64 " ++ show a
    show (PInt64 a)        = "PInt64 " ++ show a
    show (PText a)         = "PText " ++ show a
    show (PBytes a)        = "PBytes " ++ show a
    show (PURI a)          = "PURI " ++ show a
    show (PUUID a)         = "PUUID " ++ show a
    show (PTime a)         = "PTime " ++ show a
    show (PDate a)         = "PDate " ++ show a
    show (PPeriod a)       = "PPeriod " ++ show a
    show (PInstance a)     = "PInstance " ++ show a
    show (PPartition a)    = "PPartition " ++ show a
    show (PEntity a)       = "PEntity " ++ show a
    show (PTransaction a)  = "PTransaction " ++ show a
    show (PAttribute a)    = "PAttribute " ++ show a

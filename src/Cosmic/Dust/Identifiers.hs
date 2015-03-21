{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE UndecidableInstances#-}
module Cosmic.Dust.Identifiers where

import Prelude(Eq(..),Ord(..),Integer,Show(..),(&&))
import Data.Word(Word8,Word16,Word32,Word64)

type family DatabaseIDs t kind :: *

newtype TransactionK  = TransactionK ()
newtype AttributeK    = AttributeK ()
newtype PartitionK    = PartitionK ()
newtype EntityK       = EntityK ()
newtype EntityRefK    = EntityRefK ()


data SimpleSmallDatabase
data ComplexSmallDatabase
data ComplexDatabase

type instance DatabaseIDs SimpleSmallDatabase TransactionK  = ()
type instance DatabaseIDs SimpleSmallDatabase AttributeK    = Word8
type instance DatabaseIDs SimpleSmallDatabase PartitionK    = Word8
type instance DatabaseIDs SimpleSmallDatabase EntityK       = Word16
type instance DatabaseIDs SimpleSmallDatabase EntityRefK    = (Word8, Word16)

type instance DatabaseIDs ComplexSmallDatabase TransactionK = Integer
type instance DatabaseIDs ComplexSmallDatabase AttributeK   = Word32
type instance DatabaseIDs ComplexSmallDatabase PartitionK   = Word8
type instance DatabaseIDs ComplexSmallDatabase EntityK      = Integer
type instance DatabaseIDs ComplexSmallDatabase EntityRefK   = (Word8, Integer)

type instance DatabaseIDs ComplexDatabase TransactionK      = Integer
type instance DatabaseIDs ComplexDatabase AttributeK        = Word32
type instance DatabaseIDs ComplexDatabase PartitionK        = Word32
type instance DatabaseIDs ComplexDatabase EntityK           = Integer
type instance DatabaseIDs ComplexDatabase EntityRefK        = (Word32, Integer)

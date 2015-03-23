{-# LANGUAGE NoImplicitPrelude #-}
module Cosmic.Dust.Value
  ( PrimitiveParticle(..)
  , ParticleKind(..)
  , ContextualParticle(..)
  )
where

-- Base types
import Prelude(Integer,Float,Double,Bool(..))
import Data.Word(Word8,Word16,Word32,Word64)
import Data.Int

-- Type classes from Base
import Prelude(Eq(..),Show(..))
import Data.Ord

-- Externals
import Data.Text as T
import Data.ByteString as B

data PrimitiveParticle
  =  PUnit
  |  PBool     Bool
  -- Numbers
  | PInt      Int
  | PBigInt   Integer
  | PFloat    Float
  | PDouble   Double
  -- Computery numbers
  | PWord8    Word8
  | PWord16   Word16
  | PWord32   Word32
  | PWord64   Word64
  | PInt64    Int64
  -- External basics
  | PText     T.Text
  | PBytes    B.ByteString
  deriving(Eq,Ord,Show)

data ContextualParticle
  = ContextualParticle PP PP
  deriving(Eq,Ord,Show)

-- Short aliases for internal convenience.
type PP = PrimitiveParticle
type CP = ContextualParticle

data ParticleKind
  = KindUnit
  | KindSet
  | KindSingle  PP
  | KindMap2    PP PP
  | KindMap3    PP PP PP
  | KindContext CP
  deriving(Eq,Ord,Show)

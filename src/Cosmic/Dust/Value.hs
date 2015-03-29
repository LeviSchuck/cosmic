{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Cosmic.Dust.Value
  ( PrimitiveParticle(..)
  , ParticleKind(..)
  , ContextualParticle(..)
  , constByPrim
  , constByKind
  )
where

-- Base types
import Prelude(Integer,Float,Double,Bool(..),String,undefined,($),(.))
import Data.Word(Word8,Word16,Word32,Word64)
import Data.Int
import Control.Applicative(Const(..))

-- Type classes from Base
import Prelude(Eq(..),Show(..))
import Data.Ord
import Data.Typeable
import Data.Data

-- Externals
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Map as M

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
  deriving(Eq,Ord,Show,Typeable,Data)

-- forall proxy a. Typeable a =>

constByPrim :: (a -> PrimitiveParticle) -> Const Constr b
constByPrim f = Const . toConstr $ f undefined


data ContextualParticle
  = SimpleContext PP PP
  | MappedContext (M.Map PP PP)
  deriving(Eq,Ord,Show,Typeable)

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
  deriving(Eq,Ord,Show,Typeable)

constByKind :: (a -> ParticleKind) -> Const String b
constByKind f = Const $ case f undefined of
  KindUnit          -> "Unit"
  KindSet           -> "Set"
  KindSingle  _     -> "Single"
  KindMap2    _ _   -> "Map2"
  KindMap3    _ _ _ -> "Map3"
  KindContext _     -> "Context"

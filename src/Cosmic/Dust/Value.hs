{-# LANGUAGE
    NoImplicitPrelude
  , DeriveDataTypeable
  , ScopedTypeVariables
  #-}

{-|

Most of the primitives in 'PrimitiveParticle' already
have an instance of 'Cosmic.Dust.Extract' that you can use.

Ideally you can use these primitives to extend your own
flattened wire / storage format.

-}
module Cosmic.Dust.Value
  ( PrimitiveParticle(..)
  , ParticleKind(..)
  , ContextualParticle(..)
  , constByPrim
  , constByKind
  , constByTypeable
  , PP
  , CP
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

-- | An algebraic data type that holds the various
-- primitives that can be stored and sent agnostically.
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

-- | Describes the constructor given.
-- Most of the primitive constructors take just one value.
-- 
-- > expectedPrim = constByPrim PWord64
-- 
-- Although unlikely, the way to get around 'PUnit' not taking
-- any values is to encode it as a function taking a parameter.
--
-- > expectedPrim = constByPrim $ \_ -> PUnit
-- 
constByPrim :: (a -> PrimitiveParticle) -> Const Constr b
constByPrim f = Const . toConstr $ f undefined


-- | Useful wrapper for meta information
data ContextualParticle
  = SimpleContext PP PP
  | MappedContext (M.Map PP PP)
  deriving(Eq,Ord,Show,Typeable,Data)

-- | Short convenience alias
type PP = PrimitiveParticle
-- | Short convenience alias
type CP = ContextualParticle

-- | Direct particle wrapper.
--
-- Set and Map values can be used in pre-values
-- for association in 'Cosmic.Dust.Fact's
data ParticleKind
  = KindUnit
  | KindSet
  | KindSingle  PP
  | KindMap2    PP PP
  | KindMap3    PP PP PP
  | KindContext CP
  deriving(Eq,Ord,Show,Typeable,Data)

-- | Describes the constructor given for a direct particle.
constByKind :: (a -> ParticleKind) -> Const String b
constByKind f = Const $ case f undefined of
  KindUnit          -> "Unit"
  KindSet           -> "Set"
  KindSingle  _     -> "Single"
  KindMap2    _ _   -> "Map2"
  KindMap3    _ _ _ -> "Map3"
  KindContext _     -> "Context"

-- | Given a "Data.Typeable" 'a', we can automate the name
-- by getting the type constructor.
--
-- > extractNameKind = constByKind KindMap2
--
-- Although unlikely, you can get around 'KindUnit'
-- and 'KindSet's lack of any values by doing
-- something like this:
-- 
-- > extractNameKind = constByKind $ \_ -> KindUnit
-- 
constByTypeable :: forall a. Typeable a
                => Const String a
constByTypeable = Const $
  tyConName.typeRepTyCon.typeRep $ (Proxy :: Proxy a)

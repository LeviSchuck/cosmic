{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cosmic.Dust.Extract
  ( Extract(..)
  , UnsafeExtract(..)
  , badPrim
  , badKind
  , extractPrimitive
  , constByTypeable
  , constByPrim
  , constByKind
  ) where
-- Base
import Data.Maybe
import Data.Either
import Prelude(String,(++),Show(..),($),(.),undefined,Eq(..))
import Data.Typeable
import Data.Data
import Control.Applicative(Const(..))

-- Internal
import Cosmic.Dust.Value

-- Base types
import Prelude(Integer,Float,Double,Bool(..))
import Data.Word(Word8,Word16,Word32,Word64)
import Data.Int

-- Externals
import Data.Text as T
import Data.ByteString as B

class UnsafeExtract a where
  unsafeExtract :: PrimitiveParticle -> a

instance UnsafeExtract () where
  unsafeExtract PUnit = ()
  unsafeExtract _ = undefined

instance UnsafeExtract Bool where
  unsafeExtract (PBool v) = v
  unsafeExtract _ = undefined

instance UnsafeExtract Int where
  unsafeExtract (PInt v) = v
  unsafeExtract _ = undefined

instance UnsafeExtract Integer where
  unsafeExtract (PBigInt v) = v
  unsafeExtract _ = undefined

instance UnsafeExtract Float where
  unsafeExtract (PFloat v) = v
  unsafeExtract _ = undefined

instance UnsafeExtract Double where
  unsafeExtract (PDouble v) = v
  unsafeExtract _ = undefined

instance UnsafeExtract Word8 where
  unsafeExtract (PWord8 v) = v
  unsafeExtract _ = undefined

instance UnsafeExtract Word16 where
  unsafeExtract (PWord16 v) = v
  unsafeExtract _ = undefined

instance UnsafeExtract Word32 where
  unsafeExtract (PWord32 v) = v
  unsafeExtract _ = undefined

instance UnsafeExtract Word64 where
  unsafeExtract (PWord64 v) = v
  unsafeExtract _ = undefined

instance UnsafeExtract Int64 where
  unsafeExtract (PInt64 v) = v
  unsafeExtract _ = undefined

instance UnsafeExtract T.Text where
  unsafeExtract (PText v) = v
  unsafeExtract _ = undefined

instance UnsafeExtract B.ByteString where
  unsafeExtract (PBytes v) = v
  unsafeExtract _ = undefined

{-|
Extract is a way to take a raw "PartkcleKind" and
yield a value 'a'.

Most primitives already have been implemented 
-}
class Extract a where
  -- Types
  extractMaybe      :: ParticleKind -> Maybe a
  extractDefault    :: a -> ParticleKind -> a
  extractEither     :: ParticleKind -> Either String a
  extractParticle   :: PrimitiveParticle -> Either String a
  expectedPrim      :: Const Constr a
  extractNamePrim   :: Const String a
  extractNameKind   :: Const String a
  extractNameType   :: Const String a
  -- Default implementations
  extractMaybe particle =
    case extractEither particle of
      Left _ -> Nothing
      Right v -> Just v

  extractDefault def particle =
    case extractMaybe particle of
      Nothing -> def
      Just v -> v

  extractEither (KindSingle prim) = extractParticle prim
  extractEither k = badKind k

  extractNameKind = constByKind KindSingle

  extractNamePrim
    = Const
    . showConstr
    . getConst
    $ (expectedPrim :: Const Constr a)

-- Now for the primitives!

badKind :: forall a. Extract a
        => ParticleKind
        -> Either String a
badKind kind = Left $
  "Expected "
  ++ getConst exKind
  ++ " kind, found "
  ++ show kind
  where
    exKind :: Const String a
    exKind = extractNameKind

badPrim :: forall a. Extract a
        => PrimitiveParticle
        -> Either String a
badPrim prim = Left $
  "Expected a " 
  ++ getConst exPrim
  ++ " for "
  ++ getConst exType
  ++ ", found "
  ++ show prim
  where
    exPrim :: Const String a
    exPrim = extractNamePrim
    exType :: Const String a
    exType = extractNameType

constByTypeable :: forall a. Typeable a
                => Const String a
constByTypeable = Const $
  tyConName.typeRepTyCon.typeRep $ (Proxy :: Proxy a)

extractPrimitive  :: forall a. (UnsafeExtract a, Extract a)
                  => PrimitiveParticle
                  -> Either String a
extractPrimitive prim = if tcr == etcr
    then Right $ unsafeExtract prim
    else badPrim prim
    where
      etcr = getConst (expectedPrim :: Const Constr a)
      tcr = toConstr prim


instance Extract () where
  expectedPrim = constByPrim $ \_ -> PUnit
  extractNameKind = constByKind $ \_ -> KindUnit
  extractNameType = constByTypeable
  extractParticle = extractPrimitive



instance Extract Bool where
  expectedPrim    = constByPrim PBool
  extractNameType = constByTypeable
  extractParticle = extractPrimitive

instance Extract Int where
  expectedPrim = constByPrim PInt
  extractNameType = constByTypeable
  extractParticle = extractPrimitive

instance Extract Integer where
  expectedPrim = constByPrim PBigInt
  extractNameType = constByTypeable
  extractParticle = extractPrimitive

instance Extract Float where
  expectedPrim = constByPrim PFloat
  extractNameType = constByTypeable
  extractParticle = extractPrimitive

instance Extract Double where
  expectedPrim = constByPrim PDouble
  extractNameType = constByTypeable
  extractParticle = extractPrimitive

instance Extract Word8 where
  expectedPrim = constByPrim PWord8
  extractNameType = constByTypeable
  extractParticle = extractPrimitive

instance Extract Word16 where
  expectedPrim = constByPrim PWord16
  extractNameType = constByTypeable
  extractParticle = extractPrimitive

instance Extract Word32 where
  expectedPrim = constByPrim PWord32
  extractNameType = constByTypeable
  extractParticle = extractPrimitive

instance Extract Word64 where
  expectedPrim = constByPrim PWord64
  extractNameType = constByTypeable
  extractParticle = extractPrimitive

instance Extract Int64 where
  expectedPrim = constByPrim PInt64
  extractNameType = constByTypeable
  extractParticle = extractPrimitive

instance Extract T.Text where
  expectedPrim = constByPrim PText
  extractNameType = constByTypeable
  extractParticle = extractPrimitive

instance Extract B.ByteString where
  expectedPrim = constByPrim PBytes
  extractNameType = constByTypeable
  extractParticle = extractPrimitive


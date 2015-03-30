{-# LANGUAGE
    NoImplicitPrelude
  , ScopedTypeVariables
  #-}
{-|
'Extract' is a way to take a raw 'Cosmic.Dust.PartkcleKind' and
yield a value 'a'.

Libraries can provide additional types not built in such as dates.

However, it is not recommended to put whatever you want in here
with the 'ByteString' type (Like collections).

Most primitives already have been implemented.

You can implement your new types similar to this:

@
newtype SomeID
  = SomeID
  { unID :: Word32
  } deriving(Eq,Ord,Show,Typeable,Data)

instance TaggedDbType8 TransactionID where
  getTag _ = 123

instance Extract SomeID where
  extractNameKind = constByKind KindContext
  expectedPrim    = constByPrim PWord32
  extractNameType = constByTypeable
  extractEither   = extractContext1 SomeID
  -- Disallow semi-direct extraction
  extractParticle prim = badPrim prim
@
-}
module Cosmic.Dust.Extract
  ( Extract(..)
  , badPrim
  , badKind
  ) where
-- Base
import Data.Maybe
import Data.Either
import Prelude(String,(++),Show(..),($),(.),undefined,Eq(..))
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

-- 'UnsafeExtract' is meant for primitive types, be careful.
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

-- | Typeclass for conversion from
-- 'ParticleKind' and 'ContextualParticle' to 'a'
class Extract a where
  -- Types
  -- | Attempts to extract 'a'
  extractMaybe      :: ParticleKind 
                    -- ^ Input Particle
                    -> Maybe a

  -- | This uses 'extractMaybe', if it fails, it gives the default
  extractDefault    :: a
                    -- ^ Default value
                    -> ParticleKind
                    -- ^ Input Particle
                    -> a

  -- | 'extractEither' is implemented for 'KindSingle'
  -- direct particles, using 'extractParticle'.
  extractEither     :: ParticleKind
                    -- ^ Input Direct Particle
                    -> Either String a
                    -- ^ Output value or failure text

  -- | Takes a primitive and gives the expected type 'a'
  extractParticle   :: ContextualParticle
                    -- ^ Input Primitive Particle
                    -> Either String a
                    -- ^ Output value or failure text

  -- | Describes the constructor that is expected
  -- and is used in failure text by default
  expectedPrim      :: Const Constr a

  -- | The particle primitive name used during extraction.
  -- This is implemented by default using "expectedPrim"
  extractNamePrim   :: Const String a

  -- | Describes which kind is used, helpful for failure text.
  extractNameKind   :: Const String a

  -- | Describes the type 'a' for failure text.
  -- if 'a' is 'Data.Typeable', then you can implement this with
  -- @
  -- extractNameType = constByTypeable
  -- @
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

-- | Use this to give failure text when an unexpected
-- direct particle is encountered.
-- 
-- @
-- extractEither (KindMap2 p1 p2) = ...
-- extractEither k = badKind k
-- @
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

-- | Use this to give failure text when an unexpected
-- direct particle is encountered.
badPrim :: forall a. Extract a
        => ContextualParticle
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

extractSemiPrimitive  :: forall a. (UnsafeExtract a, Extract a)
                      => ContextualParticle
                      -> Either String a
extractSemiPrimitive (NoContext prim) = extractPrimitive prim
extractSemiPrimitive prim = badPrim prim

extractPrimitive  :: forall a. (UnsafeExtract a, Extract a)
                  => PrimitiveParticle
                  -> Either String a
extractPrimitive prim = if tcr == etcr
    then Right $ unsafeExtract prim
    else badPrim (NoContext prim)
    where
      etcr = getConst (expectedPrim :: Const Constr a)
      tcr = toConstr prim


instance Extract () where
  expectedPrim    = constByPrim $ \_ -> PUnit
  extractNameKind = constByKind $ \_ -> KindUnit
  extractNameType = constByTypeable
  extractParticle = extractSemiPrimitive

instance Extract Bool where
  expectedPrim    = constByPrim PBool
  extractNameType = constByTypeable
  extractParticle = extractSemiPrimitive

instance Extract Int where
  expectedPrim = constByPrim PInt
  extractNameType = constByTypeable
  extractParticle = extractSemiPrimitive

instance Extract Integer where
  expectedPrim = constByPrim PBigInt
  extractNameType = constByTypeable
  extractParticle = extractSemiPrimitive

instance Extract Float where
  expectedPrim = constByPrim PFloat
  extractNameType = constByTypeable
  extractParticle = extractSemiPrimitive

instance Extract Double where
  expectedPrim = constByPrim PDouble
  extractNameType = constByTypeable
  extractParticle = extractSemiPrimitive

instance Extract Word8 where
  expectedPrim = constByPrim PWord8
  extractNameType = constByTypeable
  extractParticle = extractSemiPrimitive

instance Extract Word16 where
  expectedPrim = constByPrim PWord16
  extractNameType = constByTypeable
  extractParticle = extractSemiPrimitive

instance Extract Word32 where
  expectedPrim = constByPrim PWord32
  extractNameType = constByTypeable
  extractParticle = extractSemiPrimitive

instance Extract Word64 where
  expectedPrim = constByPrim PWord64
  extractNameType = constByTypeable
  extractParticle = extractSemiPrimitive

instance Extract Int64 where
  expectedPrim = constByPrim PInt64
  extractNameType = constByTypeable
  extractParticle = extractSemiPrimitive

instance Extract T.Text where
  expectedPrim = constByPrim PText
  extractNameType = constByTypeable
  extractParticle = extractSemiPrimitive

instance Extract B.ByteString where
  expectedPrim = constByPrim PBytes
  extractNameType = constByTypeable
  extractParticle = extractSemiPrimitive


{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cosmic.Dust.Extract
  ( Extract(..)
  , badPrim
  , badKind
  ) where
-- Base
import Data.Maybe
import Data.Either
import Prelude(String,(++),undefined,Show(..))
-- Internal
import Cosmic.Dust.Value

-- Base types
import Prelude(Integer,Float,Double,Bool(..))
import Data.Word(Word8,Word16,Word32,Word64)
import Data.Int
-- Externals
import Data.Text as T
import Data.ByteString as B


class Extract a where
  -- Types
  extractMaybe      :: ParticleKind -> Maybe a
  extractDefault    :: a -> ParticleKind -> a
  extractEither     :: ParticleKind -> Either String a
  extractParticle   :: PrimitiveParticle -> Either String a
  extractMap2Maybe  :: ParticleKind -> Maybe (a,a)
  extractMap3Maybe  :: ParticleKind -> Maybe (a,a,a)
  extractMap2Either :: ParticleKind -> Either String (a,a)
  extractMap3Either :: ParticleKind -> Either String (a,a,a)
  extractName       :: a -> String
  -- Default implimentations
  extractMaybe particle =
    case extractEither particle of
      Left _ -> Nothing
      Right v -> Just v

  extractDefault def particle =
    case extractMaybe particle of
      Nothing -> def
      Just v -> v

  extractEither (KindSingle prim) = extractParticle prim
  extractEither k = badKind "Single" k

  extractMap2Maybe particle =
    case extractMap2Either particle of
      Left _ -> Nothing
      Right v -> Just v

  extractMap3Maybe particle = 
    case extractMap3Either particle of
      Left _ -> Nothing
      Right v -> Just v

  extractMap2Either (KindMap2 p1 p2) =
    case extractParticle p1 of
      Left _ -> badPrim (tName ++ " in (<1>,_)") p1
      Right p1v -> case extractParticle p2 of
        Left _ -> badPrim (tName ++ " in (_,<2>)") p2
        Right p2v -> Right (p1v, p2v)
    where tName = extractName (undefined :: a)
  extractMap2Either k = badKind "Map2" k

  extractMap3Either (KindMap3 p1 p2 p3) =
    case extractParticle p1 of
      Left _ -> badPrim (tName ++ " in (<1>,_,_)") p1
      Right p1v -> case extractParticle p2 of
        Left _ -> badPrim (tName ++ " in (_,<2>,_)") p2
        Right p2v -> case extractParticle p3 of
          Left _ -> badPrim (tName ++ " in (_,_,<3>)") p3
          Right p3v -> Right (p1v, p2v, p3v)
    where tName = extractName (undefined :: a)
  extractMap3Either k = badKind "Map3" k

-- Now for the primitives!

badKind :: Show a => String -> a -> Either String b
badKind exType kind = Left
  ("Expected "
    ++ exType
    ++ " kind, found "
    ++ show kind)

badPrim :: Show a => String -> a -> Either String b
badPrim exType prim = Left
  ("Expected a " 
    ++ exType
    ++ ", found "
    ++ show prim)

instance Extract () where
  extractName _ = "Unit"
  extractParticle PUnit = Right ()
  extractParticle prim = badPrim "Unit" prim

instance Extract Bool where
  extractName _ = "Bool"
  extractParticle (PBool v) = Right v
  extractParticle prim = badPrim "Bool" prim

instance Extract Int where
  extractName _ = "Int"
  extractParticle (PInt v) = Right v
  extractParticle prim = badPrim "Int" prim

instance Extract Integer where
  extractName _ = "BigInt"
  extractParticle (PBigInt v) = Right v
  extractParticle prim = badPrim "BigInt" prim

instance Extract Float where
  extractName _ = "Float"
  extractParticle (PFloat v) = Right v
  extractParticle prim = badPrim "Float" prim

instance Extract Double where
  extractName _ = "Double"
  extractParticle (PDouble v) = Right v
  extractParticle prim = badPrim "Double" prim

instance Extract Word8 where
  extractName _ = "Word8"
  extractParticle (PWord8 v) = Right v
  extractParticle prim = badPrim "Word8" prim

instance Extract Word16 where
  extractName _ = "Word16"
  extractParticle (PWord16 v) = Right v
  extractParticle prim = badPrim "Word16" prim

instance Extract Word32 where
  extractName _ = "Word32"
  extractParticle (PWord32 v) = Right v
  extractParticle prim = badPrim "Word32" prim

instance Extract Word64 where
  extractName _ = "Word64"
  extractParticle (PWord64 v) = Right v
  extractParticle prim = badPrim "Word64" prim

instance Extract Int64 where
  extractName _ = "Int64"
  extractParticle (PInt64 v) = Right v
  extractParticle prim = badPrim "Int64" prim

instance Extract T.Text where
  extractName _ = "Text"
  extractParticle (PText v) = Right v
  extractParticle prim = badPrim "Text" prim

instance Extract B.ByteString where
  extractName _ = "Bytes"
  extractParticle (PBytes v) = Right v
  extractParticle prim = badPrim "Bytes" prim


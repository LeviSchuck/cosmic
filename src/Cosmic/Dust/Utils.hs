{-# LANGUAGE NoImplicitPrelude #-}
module Cosmic.Dust.Utils where

import Prelude(String,($),(++),show,(==),undefined)
import Data.Either
import Data.Maybe
import Data.Word
import Control.Monad
import Control.Applicative((<$>))


import Cosmic.Dust.Value
import Cosmic.Dust.Extract

-- Externals

import qualified Data.Map as M

class EnumWord8 a where
  toWord8   :: a -> Word8
  fromWord8 :: Word8 -> Maybe a

class TaggedDbType8 a where
  getTag :: a -> Word8
  getTagPrim :: a -> PrimitiveParticle
  getTagPrim v = PWord8 $ getTag v

extractEnumWord8  :: EnumWord8 a
                  => PrimitiveParticle
                  -> Either String a
extractEnumWord8 t = case t of
    PWord8 w -> case fromWord8 w of
      Just r -> Right r
      Nothing -> Left $
        "Given word ("
          ++ show w
          ++ ") is out of range"
    _ -> badPrim "Assertion as Word8" t

ensureWord8 :: (TaggedDbType8 t)
            => t
            -> PrimitiveParticle 
            -> Either String ()
ensureWord8 w (PWord8 w1) = if w' == w1
  then Right ()
  else Left $
    "Expected tag " 
    ++ (show w')
    ++ " got "
    ++ (show w1)
    where w' = getTag w
ensureWord8 _ p = badPrim "word8 tag" p

expectedTaggedKey :: TaggedDbType8 a
                  => a
                  -> String
expectedTaggedKey v
  = "Expected to find key "
    ++ show (getTag v)
    ++ " but such was not found."

extractContext1 :: (Extract a, TaggedDbType8 t)
                => (a -> t)
                -> ParticleKind
                -> Either String t
extractContext1 c (KindContext p) = do
   c <$> case p of
    SimpleContext u v -> do
      ensureWord8 dummy u
      extractParticle v
    MappedContext m -> case M.lookup (getTagPrim dummy) m of
      Nothing -> Left $ expectedTaggedKey dummy
      Just v -> extractParticle v
  where dummy = c undefined
extractContext1 _ k = badKind "KindContext " k

extractTaggedEnum :: (Extract a, TaggedDbType8 a, EnumWord8 t)
                  => a
                  -> ParticleKind
                  -> Either String t
extractTaggedEnum a (KindContext p) = do
  v <- case p of
    SimpleContext u v -> do
      ensureWord8 a u
      Right v
    MappedContext m -> case M.lookup (getTagPrim a) m of
      Nothing -> Left $ expectedTaggedKey a
      Just v -> Right v
  case v of
      PWord8 w -> case fromWord8 w of
        Nothing -> Left $ expectedTaggedKey a
        Just f -> Right f
      _ -> badPrim "Word8 as Enum" v
extractTaggedEnum _ k = badKind "KindContext " k
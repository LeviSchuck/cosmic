{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cosmic.Dust.Utils where

import Prelude(String,($),(++),show,(==),(.))
import Data.Either
import Data.Maybe
import Data.Word
import Control.Applicative((<$>),Const(..))


import Cosmic.Dust.Value
import Cosmic.Dust.Extract

-- Externals

import qualified Data.Map as M

class EnumWord8 a where
  toWord8   :: a -> Word8
  fromWord8 :: Word8 -> Maybe a

class TaggedDbType8 a where
  getTag :: Const Word8 a

getTagPrim  :: forall a. TaggedDbType8 a
            => Const PrimitiveParticle a
getTagPrim = Const . PWord8 $ getConst (getTag :: Const Word8 a)

extractEnumWord8  ::  ( Extract a
                      , EnumWord8 a
                      )
                  => PrimitiveParticle
                  -> Either String a
extractEnumWord8 t = case t of
    PWord8 w -> case fromWord8 w of
      Just r -> Right r
      Nothing -> Left $
        "Given word ("
          ++ show w
          ++ ") is out of range"
    _ -> badPrim t

ensureWord8 :: forall t. (TaggedDbType8 t, Extract t)
            => Const () t
            -> PrimitiveParticle 
            -> Either String ()
ensureWord8 _ (PWord8 w1) = if w == w1
  then Right ()
  else Left $
    "Expected tag " 
    ++ (show w)
    ++ " got "
    ++ (show w1)
    where w = getConst (getTag :: Const Word8 t)
ensureWord8 _ p = badPrim p

expectedTaggedKey :: forall a. TaggedDbType8 a
                  => Const String a
expectedTaggedKey = Const $
    "Expected to find key "
    ++ show (getConst (getTag :: Const Word8 a))
    ++ " but such was not found."

extractContext1 :: forall a t.
                    ( Extract a
                    , Extract t
                    , TaggedDbType8 t
                    )
                => (a -> t)
                -> ParticleKind
                -> Either String t
extractContext1 c (KindContext p) = do
   c <$> case p of
    SimpleContext u v -> do
      ew8 u
      extractParticle v
    MappedContext m -> case M.lookup primtag m of
      Nothing -> Left $ expected
      Just v -> extractParticle v
  where
    primtag = getConst (getTagPrim :: Const PrimitiveParticle t)
    expected = getConst (expectedTaggedKey :: Const String t)
    ew8 = ensureWord8 (Const () :: Const () t)
extractContext1 _ k = badKind k

extractTaggedEnum ::  forall a t.
                      ( Extract a
                      , Extract t
                      , TaggedDbType8 a
                      , EnumWord8 t
                      )
                  => Const () a
                  -> ParticleKind
                  -> Either String t
extractTaggedEnum _ (KindContext p) = do
  v <- case p of
    SimpleContext u v -> do
      ew8 u
      Right v
    MappedContext m -> case M.lookup primtag m of
      Nothing -> Left $ expected
      Just v -> Right v
  case v of
      PWord8 w -> case fromWord8 w of
        Nothing -> Left $ expected
        Just f -> Right f
      _ -> badPrim v
  where
    primtag = getConst (getTagPrim :: Const PrimitiveParticle a)
    ew8 = ensureWord8 (Const () :: Const () a)
    expected = getConst (expectedTaggedKey :: Const String a)
extractTaggedEnum _ k = badKind k

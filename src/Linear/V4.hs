{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}
#endif

#ifndef MIN_VERSION_vector
#define MIN_VERSION_vector(x,y,z) 1
#endif
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2012-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- 4-D Vectors
----------------------------------------------------------------------------
module Linear.V4
  ( V4(..)
  , vector, point, normalizePoint
  ) where

import Control.Applicative
import Control.DeepSeq (NFData(rnf))
import Control.Monad (liftM)
import Control.Monad.Fix
import Control.Monad.Zip
--import Control.Lens hiding ((<.>))
import Data.Binary as Binary
import Data.Bytes.Serial
import Data.Data
import Data.Distributive
import Data.Foldable
import Data.Functor.Bind
import Data.Functor.Classes
import Data.Functor.Rep
import Data.Hashable
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Serialize as Cereal
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))
import GHC.Arr (Ix(..))
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
import GHC.Generics (Generic)
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
import GHC.Generics (Generic1)
#endif
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed.Base as U
import Linear.Epsilon
import Linear.Metric
import Linear.V2
import Linear.V3
import Linear.Vector

{-# ANN module "HLint: ignore Reduce duplication" #-}

-- | A 4-dimensional vector.
data V4 a = V4 !a !a !a !a deriving (Eq,Ord,Show,Read,Data,Typeable
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
                                    ,Generic
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
                                    ,Generic1
#endif
                                    )

instance Functor V4 where
  fmap f (V4 a b c d) = V4 (f a) (f b) (f c) (f d)
  {-# INLINE fmap #-}
  a <$ _ = V4 a a a a
  {-# INLINE (<$) #-}

instance Foldable V4 where
  foldMap f (V4 a b c d) = f a `mappend` f b `mappend` f c `mappend` f d
  {-# INLINE foldMap #-}

instance Traversable V4 where
  traverse f (V4 a b c d) = V4 <$> f a <*> f b <*> f c <*> f d
  {-# INLINE traverse #-}

instance Foldable1 V4 where
  foldMap1 f (V4 a b c d) = f a <> f b <> f c <> f d
  {-# INLINE foldMap1 #-}


instance Applicative V4 where
  pure a = V4 a a a a
  {-# INLINE pure #-}
  V4 a b c d <*> V4 e f g h = V4 (a e) (b f) (c g) (d h)
  {-# INLINE (<*>) #-}

instance Apply V4 where
  V4 a b c d <.> V4 e f g h = V4 (a e) (b f) (c g) (d h)
  {-# INLINE (<.>) #-}

instance Additive V4 where
  zero = pure 0
  {-# INLINE zero #-}
  liftU2 = liftA2
  {-# INLINE liftU2 #-}
  liftI2 = liftA2
  {-# INLINE liftI2 #-}

instance Bind V4 where
  V4 a b c d >>- f = V4 a' b' c' d' where
    V4 a' _ _ _ = f a
    V4 _ b' _ _ = f b
    V4 _ _ c' _ = f c
    V4 _ _ _ d' = f d
  {-# INLINE (>>-) #-}

instance Monad V4 where
  return a = V4 a a a a
  {-# INLINE return #-}
  V4 a b c d >>= f = V4 a' b' c' d' where
    V4 a' _ _ _ = f a
    V4 _ b' _ _ = f b
    V4 _ _ c' _ = f c
    V4 _ _ _ d' = f d
  {-# INLINE (>>=) #-}

instance Num a => Num (V4 a) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (*) = liftA2 (*)
  {-# INLINE (-) #-}
  (-) = liftA2 (-)
  {-# INLINE (*) #-}
  negate = fmap negate
  {-# INLINE negate #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}

instance Fractional a => Fractional (V4 a) where
  recip = fmap recip
  {-# INLINE recip #-}
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}

instance Floating a => Floating (V4 a) where
    pi = pure pi
    {-# INLINE pi #-}
    exp = fmap exp
    {-# INLINE exp #-}
    sqrt = fmap sqrt
    {-# INLINE sqrt #-}
    log = fmap log
    {-# INLINE log #-}
    (**) = liftA2 (**)
    {-# INLINE (**) #-}
    logBase = liftA2 logBase
    {-# INLINE logBase #-}
    sin = fmap sin
    {-# INLINE sin #-}
    tan = fmap tan
    {-# INLINE tan #-}
    cos = fmap cos
    {-# INLINE cos #-}
    asin = fmap asin
    {-# INLINE asin #-}
    atan = fmap atan
    {-# INLINE atan #-}
    acos = fmap acos
    {-# INLINE acos #-}
    sinh = fmap sinh
    {-# INLINE sinh #-}
    tanh = fmap tanh
    {-# INLINE tanh #-}
    cosh = fmap cosh
    {-# INLINE cosh #-}
    asinh = fmap asinh
    {-# INLINE asinh #-}
    atanh = fmap atanh
    {-# INLINE atanh #-}
    acosh = fmap acosh
    {-# INLINE acosh #-}

instance Metric V4 where
  dot (V4 a b c d) (V4 e f g h) = a * e + b * f + c * g + d * h
  {-# INLINE dot #-}

instance Distributive V4 where
  distribute f = V4 (fmap (\(V4 x _ _ _) -> x) f)
                    (fmap (\(V4 _ y _ _) -> y) f)
                    (fmap (\(V4 _ _ z _) -> z) f)
                    (fmap (\(V4 _ _ _ w) -> w) f)
  {-# INLINE distribute #-}

instance Hashable a => Hashable (V4 a) where
  hashWithSalt s (V4 a b c d) = s `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d
  {-# INLINE hashWithSalt #-}


instance Storable a => Storable (V4 a) where
  sizeOf _ = 4 * sizeOf (undefined::a)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined::a)
  {-# INLINE alignment #-}
  poke ptr (V4 x y z w) = do poke ptr' x
                             pokeElemOff ptr' 1 y
                             pokeElemOff ptr' 2 z
                             pokeElemOff ptr' 3 w
    where ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = V4 <$> peek ptr' <*> peekElemOff ptr' 1
                <*> peekElemOff ptr' 2 <*> peekElemOff ptr' 3
    where ptr' = castPtr ptr
  {-# INLINE peek #-}

-- | Convert a 3-dimensional affine vector into a 4-dimensional homogeneous vector.
vector :: Num a => V3 a -> V4 a
vector (V3 a b c) = V4 a b c 0
{-# INLINE vector #-}

-- | Convert a 3-dimensional affine point into a 4-dimensional homogeneous vector.
point :: Num a => V3 a -> V4 a
point (V3 a b c) = V4 a b c 1
{-# INLINE point #-}

-- | Convert 4-dimensional projective coordinates to a 3-dimensional
-- point. This operation may be denoted, @euclidean [x:y:z:w] = (x\/w,
-- y\/w, z\/w)@ where the projective, homogenous, coordinate
-- @[x:y:z:w]@ is one of many associated with a single point @(x\/w,
-- y\/w, z\/w)@.
normalizePoint :: Fractional a => V4 a -> V3 a
normalizePoint (V4 a b c w) = (1/w) *^ V3 a b c
{-# INLINE normalizePoint #-}

instance Epsilon a => Epsilon (V4 a) where
  nearZero = nearZero . quadrance
  {-# INLINE nearZero #-}

instance Ix a => Ix (V4 a) where
  {-# SPECIALISE instance Ix (V4 Int) #-}

  range (V4 l1 l2 l3 l4,V4 u1 u2 u3 u4) =
    [V4 i1 i2 i3 i4 | i1 <- range (l1,u1)
                    , i2 <- range (l2,u2)
                    , i3 <- range (l3,u3)
                    , i4 <- range (l4,u4)
                    ]
  {-# INLINE range #-}

  unsafeIndex (V4 l1 l2 l3 l4,V4 u1 u2 u3 u4) (V4 i1 i2 i3 i4) =
    unsafeIndex (l4,u4) i4 + unsafeRangeSize (l4,u4) * (
    unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
    unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) *
    unsafeIndex (l1,u1) i1))
  {-# INLINE unsafeIndex #-}

  inRange (V4 l1 l2 l3 l4,V4 u1 u2 u3 u4) (V4 i1 i2 i3 i4) =
    inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
    inRange (l3,u3) i3 && inRange (l4,u4) i4
  {-# INLINE inRange #-}


data instance U.Vector    (V4 a) =  V_V4 {-# UNPACK #-} !Int !(U.Vector    a)
data instance U.MVector s (V4 a) = MV_V4 {-# UNPACK #-} !Int !(U.MVector s a)
instance U.Unbox a => U.Unbox (V4 a)

instance U.Unbox a => M.MVector U.MVector (V4 a) where
  basicLength (MV_V4 n _) = n
  basicUnsafeSlice m n (MV_V4 _ v) = MV_V4 n (M.basicUnsafeSlice (4*m) (4*n) v)
  basicOverlaps (MV_V4 _ v) (MV_V4 _ u) = M.basicOverlaps v u
  basicUnsafeNew n = liftM (MV_V4 n) (M.basicUnsafeNew (4*n))
  basicUnsafeRead (MV_V4 _ v) i =
    do let o = 4*i
       x <- M.basicUnsafeRead v o
       y <- M.basicUnsafeRead v (o+1)
       z <- M.basicUnsafeRead v (o+2)
       w <- M.basicUnsafeRead v (o+3)
       return (V4 x y z w)
  basicUnsafeWrite (MV_V4 _ v) i (V4 x y z w) =
    do let o = 4*i
       M.basicUnsafeWrite v o     x
       M.basicUnsafeWrite v (o+1) y
       M.basicUnsafeWrite v (o+2) z
       M.basicUnsafeWrite v (o+3) w
#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_V4 _ v) = M.basicInitialize v
#endif

instance U.Unbox a => G.Vector U.Vector (V4 a) where
  basicUnsafeFreeze (MV_V4 n v) = liftM ( V_V4 n) (G.basicUnsafeFreeze v)
  basicUnsafeThaw   ( V_V4 n v) = liftM (MV_V4 n) (G.basicUnsafeThaw   v)
  basicLength       ( V_V4 n _) = n
  basicUnsafeSlice m n (V_V4 _ v) = V_V4 n (G.basicUnsafeSlice (4*m) (4*n) v)
  basicUnsafeIndexM (V_V4 _ v) i =
    do let o = 4*i
       x <- G.basicUnsafeIndexM v o
       y <- G.basicUnsafeIndexM v (o+1)
       z <- G.basicUnsafeIndexM v (o+2)
       w <- G.basicUnsafeIndexM v (o+3)
       return (V4 x y z w)

instance MonadZip V4 where
  mzipWith = liftA2

instance MonadFix V4 where
  mfix f = V4 (let V4 a _ _ _ = f a in a)
              (let V4 _ a _ _ = f a in a)
              (let V4 _ _ a _ = f a in a)
              (let V4 _ _ _ a = f a in a)

instance Bounded a => Bounded (V4 a) where
  minBound = pure minBound
  {-# INLINE minBound #-}
  maxBound = pure maxBound
  {-# INLINE maxBound #-}

instance NFData a => NFData (V4 a) where
  rnf (V4 a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d

instance Serial1 V4 where
  serializeWith = traverse_
  deserializeWith k = V4 <$> k <*> k <*> k <*> k

instance Serial a => Serial (V4 a) where
  serialize = serializeWith serialize
  deserialize = deserializeWith deserialize

instance Binary a => Binary (V4 a) where
  put = serializeWith Binary.put
  get = deserializeWith Binary.get

instance Serialize a => Serialize (V4 a) where
  put = serializeWith Cereal.put
  get = deserializeWith Cereal.get

instance Eq1 V4 where eq1 = (==)
instance Ord1 V4 where compare1 = compare
instance Show1 V4 where showsPrec1 = showsPrec
instance Read1 V4 where readsPrec1 = readsPrec

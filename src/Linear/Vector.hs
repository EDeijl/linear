{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Linear.Vector
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Operations on free vector spaces.
-----------------------------------------------------------------------------
module Linear.Vector
  ( Additive(..)
  , negated
  , (^*)
  , (*^)
  , (^/)
  , basis
  , basisFor
  , kronecker
  ) where

import Control.Applicative
import Data.Complex
import Data.Foldable (foldMap)
import Data.Functor.Identity
import Data.HashMap.Lazy as HashMap
import Data.Hashable
import Data.IntMap as IntMap
import Data.Map as Map
import Data.Monoid (Sum(..))
import Data.Traversable (Traversable, mapAccumL)
import Linear.Instances ()

-- $setup
-- >>> import Control.Lens
-- >>> import Linear.V2

infixl 6 ^+^, ^-^
infixl 7 ^*, *^, ^/

-- | A vector is an additive group with additional structure.
class Functor f => Additive f where
  -- | The zero vector
  zero :: Num a => f a
#ifndef HLINT
  default zero :: (Applicative f, Num a) => f a
  zero = pure 0
#endif

  -- | Compute the sum of two vectors
  --
  -- >>> V2 1 2 ^+^ V2 3 4
  -- V2 4 6
  (^+^) :: Num a => f a -> f a -> f a
#ifndef HLINT
  default (^+^) :: Num a => f a -> f a -> f a
  (^+^) = liftU2 (+)
  {-# INLINE (^+^) #-}
#endif

  -- | Compute the difference between two vectors
  --
  -- >>> V2 4 5 - V2 3 1
  -- V2 1 4
  (^-^) :: Num a => f a -> f a -> f a
#ifndef HLINT
  default (^-^) :: Num a => f a -> f a -> f a
  x ^-^ y = x ^+^ negated y
  {-# INLINE (^-^) #-}
#endif

  -- | Linearly interpolate between two vectors.
  lerp :: Num a => a -> f a -> f a -> f a
  lerp alpha u v = alpha *^ u ^+^ (1 - alpha) *^ v
  {-# INLINE lerp #-}

  -- | Apply a function to merge the 'non-zero' components of two vectors, unioning the rest of the values.
  --
  -- * For a dense vector this is equivalent to 'liftA2'.
  --
  -- * For a sparse vector this is equivalent to 'unionWith'.
  liftU2 :: (a -> a -> a) -> f a -> f a -> f a
#ifndef HLINT
  default liftU2 :: Applicative f => (a -> a -> a) -> f a -> f a -> f a
  liftU2 = liftA2
  {-# INLINE liftU2 #-}
#endif

  -- | Apply a function to the components of two vectors.
  --
  -- * For a dense vector this is equivalent to 'liftA2'.
  --
  -- * For a sparse vector this is equivalent to 'intersectionWith'.
  liftI2 :: (a -> b -> c) -> f a -> f b -> f c
#ifndef HLINT
  default liftI2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
  liftI2 = liftI2
  {-# INLINE liftI2 #-}
#endif


instance Additive ZipList where
  zero = ZipList []
  {-# INLINE zero #-}
  liftU2 f (ZipList xs) (ZipList ys) = ZipList (liftU2 f xs ys)
  {-# INLINE liftU2 #-}
  liftI2 = liftA2
  {-# INLINE liftI2 #-}

instance Additive [] where
  zero = []
  {-# INLINE zero #-}
  liftU2 f = go where
    go (x:xs) (y:ys) = f x y : go xs ys
    go [] ys = ys
    go xs [] = xs
  {-# INLINE liftU2 #-}
  liftI2 = zipWith
  {-# INLINE liftI2 #-}

instance Additive IntMap where
  zero = IntMap.empty
  {-# INLINE zero #-}
  liftU2 = IntMap.unionWith
  {-# INLINE liftU2 #-}
  liftI2 = IntMap.intersectionWith
  {-# INLINE liftI2 #-}

instance Ord k => Additive (Map k) where
  zero = Map.empty
  {-# INLINE zero #-}
  liftU2 = Map.unionWith
  {-# INLINE liftU2 #-}
  liftI2 = Map.intersectionWith
  {-# INLINE liftI2 #-}

instance (Eq k, Hashable k) => Additive (HashMap k) where
  zero = HashMap.empty
  {-# INLINE zero #-}
  liftU2 = HashMap.unionWith
  {-# INLINE liftU2 #-}
  liftI2 = HashMap.intersectionWith
  {-# INLINE liftI2 #-}

instance Additive ((->) b)

instance Additive Complex where
  zero = 0 :+ 0
  {-# INLINE zero #-}
  liftU2 f (a :+ b) (c :+ d) = f a c :+ f b d
  {-# INLINE liftU2 #-}
  liftI2 f (a :+ b) (c :+ d) = f a c :+ f b d
  {-# INLINE liftI2 #-}

instance Additive Identity

-- | Compute the negation of a vector
--
-- >>> negated (V2 2 4)
-- V2 (-2) (-4)
negated :: (Functor f, Num a) => f a -> f a
negated = fmap negate
{-# INLINE negated #-}

-- | Compute the left scalar product
--
-- >>> 2 *^ V2 3 4
-- V2 6 8
(*^) :: (Functor f, Num a) => a -> f a -> f a
(*^) a = fmap (a*)
{-# INLINE (*^) #-}

-- | Compute the right scalar product
--
-- >>> V2 3 4 ^* 2
-- V2 6 8
(^*) :: (Functor f, Num a) => f a -> a -> f a
f ^* a = fmap (*a) f
{-# INLINE (^*) #-}

-- | Compute division by a scalar on the right.
(^/) :: (Functor f, Fractional a) => f a -> a -> f a
f ^/ a = fmap (/a) f
{-# INLINE (^/) #-}

-- @setElement i x v@ sets the @i@'th element of @v@ to @x@.
setElement :: Traversable t => Int -> a -> t a -> t a
setElement i x = snd . mapAccumL aux 0
  where aux j y = let j' = j + 1
                      y' = if i == j then x else y
                  in j' `seq` (j', y')

-- | Produce a default basis for a vector space. If the dimensionality
-- of the vector space is not statically known, see 'basisFor'.
basis :: (Applicative t, Traversable t, Num a) => [t a]
basis = [ setElement k 1 z | k <- [0..n - 1] ]
  where z = pure 0
        n = getSum $ foldMap (const (Sum 1)) z

-- | Produce a default basis for a vector space from which the
-- argument is drawn.
basisFor :: (Traversable t, Enum a, Num a) => t a -> [t a]
basisFor v = [ setElement k 1 z | k <- [0..n-1] ]
  where z = 0 <$ v
        n = getSum $ foldMap (const (Sum 1)) v

-- | Produce a diagonal matrix from a vector.
kronecker :: (Applicative t, Num a, Traversable t) => t a -> t (t a)
kronecker v = snd $ mapAccumL aux 0 v
  where aux i e = let i' = i + 1
                  in i' `seq` (i', setElement i e z)
        z = pure 0

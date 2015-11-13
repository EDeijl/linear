{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Linear.Algebra
  ( Algebra(..)
  , Coalgebra(..)
  , multRep, unitalRep
  , comultRep, counitalRep
  ) where

--import Control.Lens hiding (index)
import Data.Functor.Rep
import Data.Complex
import Data.Void
import Linear.Vector
import Linear.Quaternion
import Linear.Conjugate
import Linear.V0
import Linear.V1
import Linear.V2
import Linear.V3
import Linear.V4

-- | An associative unital algebra over a ring
class Num r => Algebra r m where
  mult :: (m -> m -> r) -> m -> r
  unital :: r -> m -> r

multRep :: (Representable f, Algebra r (Rep f)) => f (f r) -> f r
multRep ffr = tabulate $ mult (index . index ffr)

unitalRep :: (Representable f, Algebra r (Rep f)) => r -> f r
unitalRep = tabulate . unital

instance Num r => Algebra r Void where
  mult _ _ = 0
  unital _ _ = 0


instance Num r => Algebra r () where
  mult f () = f () ()
  unital r () = r

instance (Algebra r a, Algebra r b) => Algebra r (a, b) where
  mult f (a,b) = mult (\a1 a2 -> mult (\b1 b2 -> f (a1,b1) (a2,b2)) b) a
  unital r (a,b) = unital r a * unital r b



-- | A coassociative counital coalgebra over a ring
class Num r => Coalgebra r m where
  comult :: (m -> r) -> m -> m -> r
  counital :: (m -> r) -> r

comultRep :: (Representable f, Coalgebra r (Rep f)) => f r -> f (f r)
comultRep fr = tabulate $ \i -> tabulate $ \j -> comult (index fr) i j

counitalRep :: (Representable f, Coalgebra r (Rep f)) => f r -> r
counitalRep = counital . index

instance Num r => Coalgebra r Void where
  comult _ _ _ = 0
  counital _ = 0

instance Num r => Coalgebra r () where
  comult f () () = f ()
  counital f = f ()


instance (Coalgebra r m, Coalgebra r n) => Coalgebra r (m, n) where
  comult f (a1, b1) (a2, b2) = comult (\a -> comult (\b -> f (a, b)) b1 b2) a1 a2
  counital k = counital $ \a -> counital $ \b -> k (a,b)

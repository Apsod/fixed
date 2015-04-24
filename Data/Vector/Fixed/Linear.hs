{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}

module Data.Vector.Fixed.Linear where

import Data.Vector.Fixed hiding  ((!))
import qualified Data.Vector.Fixed as Fixed
import Data.Vector.Fixed.Size
import Data.Vector.Fixed.Indexed

import Control.DeepSeq
import Data.Foldable hiding (sum) 
import Data.Traversable
import Control.Applicative
import Data.Monoid
import Data.Proxy
import Data.Distributive

import Prelude hiding (sum, maximum)

newtype Matrix (r :: Size) (c :: Size) a = Matrix{flattenMatrix :: Vector (r * c) a}
    deriving(Show, Eq, Ord, Functor, Foldable, Traversable, NFData, Applicative)

instance (Known r, Known c) => Distributive (Matrix r c) where
  distribute = Matrix . distribute . fmap flattenMatrix

instance (Known r, Known c) => Indexable (Matrix r c) where
  type Index (Matrix r c) = (Int, Int)
  indices = Matrix . generate $ \ix -> quotRem ix (getInt (Proxy :: Proxy c))
  m !? (rix,cix) = if 0   <= rix                       &&
                      rix <  getInt (Proxy :: Proxy r) &&
                      0   <= cix                       &&
                      cix <  getInt (Proxy :: Proxy r)
               then Just (m ! (rix,cix))
               else Nothing

sum :: (Foldable f, Num a) => f a -> a 
sum = foldl' (+) 0 

(!) :: forall c. Known c => forall r a. Matrix r c a -> (Int, Int) -> a
(!) = let c = getInt (Proxy :: Proxy c)
      in (\(Matrix v) (rix,cix) -> (Fixed.!) v (c*rix + cix))

ixMatrix :: forall r c.(Known c, Known (r*c)) => Matrix r c (Int, Int)
ixMatrix = let c = getInt (Proxy :: Proxy c)
           in Matrix $ generate (\ix -> ix `quotRem` c)

onRows :: (Known r, Known c) => (Vector c a -> b) -> Matrix r c a -> Vector r b
onRows f = fmap f . separate . flattenMatrix

zero :: (Known n, Num a) => Vector n a
zero = pure 0

sub :: (Known n, Num a) => Vector n a -> Vector n a -> Vector n a
sub = liftA2 (-)

add :: (Known n, Num a) => Vector n a -> Vector n a -> Vector n a
add = liftA2 (+)

dot :: (Known n, Num a) => Vector n a -> Vector n a -> a
dot a = sum . liftA2 (*) a

cossim :: (Known n, Floating a) => Vector n a -> Vector n a -> a
cossim a b = sum $ (*) <$> normalize2 a <*> normalize2 b

outer :: (Known n, Known m, Num a) => Vector n a -> Vector m a -> Matrix n m a
outer v = Matrix . flatten . traverse (\x -> (*x) <$> v)

mXv :: (Known r, Known c, Num a) => Matrix r c  a -> Vector c a -> Vector r a
mXv (Matrix m) v = dot v <$> separate m

vXm :: (Known r, Known c, Num a) => Vector r a-> Matrix r c a -> Vector c a
vXm v (Matrix m) = foldl' add zero $ (\x -> fmap (x*)) <$> v <*> separate m

transpose :: forall r c a. (Known r, Known c) => Matrix r c a -> Matrix c r a
transpose (Matrix m) = Matrix $ flatten (sequenceA . separate $ m :: Vector c (Vector r a))

norm2Square :: (Known n, Num a) => Vector n a -> a
norm2Square = getSum . foldMap (\x -> Sum (x * x))

dist2Square :: (Known n, Num a) => Vector n a -> Vector n a -> a
dist2Square a = norm2Square . sub a

norm2 :: (Known n, Floating a) => Vector n a -> a
norm2 = sqrt . norm2Square

dist2 :: (Known n, Floating a) => Vector n a -> Vector n a -> a
dist2 a = sqrt . dist2Square a

normalize2 :: (Known n, Floating a) => Vector n a -> Vector n a
normalize2 v = (/ norm2 v) <$> v

norm1 :: (Known n, Num a) => Vector n a -> a
norm1 = getSum . foldMap (Sum . abs)

dist1 :: (Known n, Num a) => Vector n a -> Vector n a -> a
dist1 a = norm1 . sub a

normalize1 :: (Known n, Fractional a) => Vector n a -> Vector n a
normalize1 v = (/ norm1 v) <$> v

normInf :: (Known n, Num a, Ord a) => Vector n a -> a
normInf = maximum . fmap abs

distInf :: (Known n, Num a, Ord a) => Vector n a -> Vector n a -> a
distInf a = normInf . sub a

normalizeInf :: (Known n, Fractional a, Ord a) => Vector n a -> Vector n a
normalizeInf v = (/ normInf v) <$> v

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

import GHC.TypeLits

import Data.Vector.Fixed

import Control.DeepSeq
import Data.Foldable
import Data.Traversable
import Control.Applicative
import Data.Monoid

import Prelude hiding (sum, maximum)

newtype Matrix (r :: Nat) (c :: Nat) a = Matrix{flattenMatrix :: Vector (r * c) a}
    deriving(Show, Eq, Ord, Functor, Foldable, Traversable, NFData)

zero :: (KnownNat n, Num a) => Vector n a
zero = pure 0

sub :: (KnownNat n, Num a) => Vector n a -> Vector n a -> Vector n a
sub = liftA2 (-) 

add :: (KnownNat n, Num a) => Vector n a -> Vector n a -> Vector n a
add = liftA2 (+)

dot :: (KnownNat n, Num a) => Vector n a -> Vector n a -> a
dot a = sum . liftA2 (*) a

cossim :: (KnownNat n, Floating a) => Vector n a -> Vector n a -> a
cossim a b = sum $ (*) <$> normalize2 a <*> normalize2 b

outer :: (KnownNat n, KnownNat m, Num a) => Vector n a -> Vector m a -> Matrix n m a
outer v = Matrix . flatten . traverse (\x -> (*x) <$> v)

mXv :: (KnownNat r, KnownNat c, Num a) => Matrix r c  a -> Vector c a -> Vector r a
mXv (Matrix m) v = dot v <$> separate m

vXm :: (KnownNat r, KnownNat c, Num a) => Vector r a-> Matrix r c a -> Vector c a
vXm v (Matrix m) = foldl' add zero $ (\x -> fmap (x*)) <$> v <*> separate m

transpose :: forall r c a. (KnownNat r, KnownNat c) => Matrix r c a -> Matrix c r a
transpose (Matrix m) = Matrix $ flatten (sequenceA . separate $ m :: Vector c (Vector r a))

norm2Square :: (KnownNat n, Num a) => Vector n a -> a
norm2Square = getSum . foldMap (\x -> Sum (x * x))

dist2Square :: (KnownNat n, Num a) => Vector n a -> Vector n a -> a
dist2Square a = norm2Square . sub a 

norm2 :: (KnownNat n, Floating a) => Vector n a -> a
norm2 = sqrt . norm2Square

dist2 :: (KnownNat n, Floating a) => Vector n a -> Vector n a -> a
dist2 a = sqrt . dist2Square a

normalize2 :: (KnownNat n, Floating a) => Vector n a -> Vector n a
normalize2 v = (/ norm2 v) <$> v

norm1 :: (KnownNat n, Num a) => Vector n a -> a
norm1 = getSum . foldMap (Sum . abs)

dist1 :: (KnownNat n, Num a) => Vector n a -> Vector n a -> a
dist1 a = norm1 . sub a

normalize1 :: (KnownNat n, Fractional a) => Vector n a -> Vector n a
normalize1 v = (/ norm1 v) <$> v

normInf :: (KnownNat n, Num a, Ord a) => Vector n a -> a
normInf = maximum . fmap abs 

distInf :: (KnownNat n, Num a, Ord a) => Vector n a -> Vector n a -> a
distInf a = normInf . sub a 

normalizeInf :: (KnownNat n, Fractional a, Ord a) => Vector n a -> Vector n a
normalizeInf v = (/ normInf v) <$> v

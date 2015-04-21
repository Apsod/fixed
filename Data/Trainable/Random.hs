{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE FlexibleContexts           #-}

module Data.Trainable.Random where

import Control.Monad.ST
import System.Random.MWC
import System.Random.MWC.CondensedTable

import Data.Traversable
import Data.Foldable
import Control.Applicative

import Data.Proxy

import qualified Data.Vector as Vector  

import GHC.TypeLits
import Data.Vector.Fixed as Fixed
import Control.Monad.State

import Data.IntSet as IntSet

import Prelude hiding(sum)

newtype Random a = Random{runRandom :: forall s. Gen s -> ST s a}

instance Functor Random where
  fmap f (Random r) = Random (\g -> f <$> r g)

instance Applicative Random where
  pure x = Random (const $ pure x)
  (Random rf) <*> (Random rx) = Random (\g -> rf g <*> rx g)

instance Monad Random where
  return = pure
  (Random rx) >>= f = Random (\g -> rx g >>= (\x -> runRandom (f x) g))

runOn :: Random a -> (forall s. Gen s) -> a
runOn rx g = runST (runRandom rx g)

runIO :: Random a -> IO a
runIO = withSystemRandom . runRandom

-- Generate n different integers from the supplied random function.
-- Will get stuck if the generator can't generate enough distinct integers
generateDistinct :: forall n.(KnownNat n) => Random Int -> Random (Vector n Int)
generateDistinct r = evalStateT (Fixed.replicateM grow) IntSet.empty
  where r' = lift r
        grow :: StateT IntSet Random Int
        grow = do
          x <- r'
          set <- get
          case IntSet.member x set of
            True -> grow
            False -> do
              put (IntSet.insert x set)
              return x

generateRandom :: (KnownNat n) => Random a -> Random (Vector n a)
generateRandom r = sequenceA $ pure r

-- Generate n different uniform integers from the range [0,m).
-- The types ensures that this function won't get stuck, but might be slow
generateDistinctUniform :: forall n m. (KnownNat n, KnownNat m, n <= m) => Proxy m -> Random (Vector n Int)
generateDistinctUniform p = let r = Random (uniformR (0, intNat p - 1))
                            in generateDistinct r

sparseVector :: (KnownNat n) => Double -> Random (Vector n Double)
sparseVector s = let table = tableFromWeights
                             $ Vector.fromList
                             [(sqrt s         , (1 / (2 * s)))
                             ,(0              , (1 - (1 / s)))
                             ,(negate (sqrt s), (1 / (2 * s)))]
                 in generateRandom (Random $ genFromTable table) 

generateUniform :: (KnownNat n) => Int -> Random (Vector n Int)
generateUniform n = generateRandom (Random $ uniformR (0, n - 1))

-- Generate a vector, and apply the permutation.
randomDistinctBackpermute :: forall n m a. (KnownNat n, KnownNat m, n <= m) => Random (Vector m a -> Vector n a)
randomDistinctBackpermute = flip Fixed.backpermute <$> generateDistinctUniform (Proxy :: Proxy m)

randomUniformBackpermute :: forall n m a. (KnownNat n, KnownNat m, n <= m) => Random (Vector m a -> Vector n a)
randomUniformBackpermute = flip Fixed.backpermute <$> generateUniform (intNat (Proxy :: Proxy m))

-- This function computes the dot product between a vector v and a random vector with n non-zero elements,
-- where each non-zero element is either 1 or -1 with equal probability.
sparseRandomDot :: forall n m a. (KnownNat n, KnownNat m, n <= m, Num a) => Proxy n -> Random (Vector m a -> a)
sparseRandomDot _ = (\randomFlip randomPermute -> sum . randomFlip . randomPermute)
                    <$> flipRandom
                    <*> (randomDistinctBackpermute :: Random (Vector m a -> Vector n a))

flipRandom :: (KnownNat n, Num a) => Random (Vector n a -> Vector n a)
flipRandom = let flipNum :: (Num a) => Random (a -> a)
                 flipNum = (\pos x -> if pos then x else negate x) <$> (Random uniform)
             in applyRandom flipNum

--This function computes the matrix-vector product between a vector v and a random matrix, where each
--row in the matrix is a random vector filled with n non-zero elements (1 or -1 with equal probability)
sparseRandomProjection :: forall n m r a. (KnownNat n, KnownNat m, KnownNat r, n <= m, Floating a)
                          => Proxy n -> Random (Vector m a -> Vector r a)
sparseRandomProjection p = let m = fromIntegral (intNat (Proxy :: Proxy m))
                               n = fromIntegral (intNat (Proxy :: Proxy n))
                               r = fromIntegral (intNat (Proxy :: Proxy r))
                               scale :: Floating a => a 
                               scale = sqrt (m / (r * n)) 
                           in fmap (fmap (*scale)) . oneToMany <$>
                              (sequenceA . pure $ sparseRandomDot p)



applyRandom :: (KnownNat n) => Random (a -> b) -> Random (Vector n a -> Vector n b)
applyRandom r = fmap (\f v -> f <*> v) <$> sequenceA $ pure r

oneToMany :: (Functor f) => f (a -> b) -> a -> f b
oneToMany fs x = fmap ($x) fs

repeatLeft :: (Applicative f) => (a -> b) -> (a -> f b)
repeatLeft f x = f <$> pure x

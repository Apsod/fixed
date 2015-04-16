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

module Data.Trainable where


import Data.Vector.Fixed

import Data.Profunctor
import Data.Foldable
import Data.Distributive 
import Control.Applicative 

import Data.Monoid

import Data.Number.Erf

import Data.Reflection
import Numeric.AD
import Numeric.AD.Internal.Reverse

import GHC.TypeLits 

import Prelude hiding (head) 

newtype Net (n :: Nat) a b = Net{unNet :: DownStar (Vector n) a b}
                           deriving(Functor, Applicative, Monad, Profunctor) 

runNet :: Net n a b -> Vector n a -> b
runNet = runDownStar . unNet

mkNet :: (Vector n a -> b) -> Net n a b
mkNet = Net . DownStar

mkNet' :: (a -> b) -> Net 1 a b 
mkNet' f = mkNet (f . head) 

instance (KnownNat n) => Distributive (Net n a) where
  distribute fs = mkNet (\ps -> (\f -> (runNet f ps)) <$> fs)

--  A version of <*> that separates the arguments
infixl 4 <+>
(<+>) :: (KnownNat m) => Net m w (a -> b) -> Net n w a -> Net (m + n) w b
f <+> x = mkNet (\v -> let (fp, xp) = split v in runNet f fp $ runNet x xp)

-- A version of >>= that separates the arguments
infixl 1 >>+
(>>+) :: (KnownNat m) => Net m w a -> (a -> Net n w b) -> Net(m + n) w b
x >>+ f = mkNet (\v -> let (xp, fp) = split v in runNet(f $ runNet x xp) fp)

-- A version of distribute that separates the arguments
explode :: (KnownNat m, KnownNat n) => Net m p a -> Net (n * m) p (Vector n a)
explode net = mkNet (\v -> let fs = separate v in runNet <$> pure net <*> fs)

oneToMany :: (Functor f) => f (a -> b) -> a -> f b
oneToMany fs x = fmap ($x) fs

fanOut :: (KnownNat m, KnownNat n) => Net m p (a -> b) -> Net (n * m) p (a -> Vector n b)
fanOut = fmap oneToMany . explode

fanOut' :: (KnownNat m) => (p -> a -> b) -> Net m p (a -> Vector m b) 
fanOut' f = mkNet (\ws x -> (\w -> f w x) <$> ws)

manyToOne :: (Foldable f, Applicative f, Monoid b) => f (a -> b) -> f a -> b
manyToOne fs xs = fold (fs <*> xs)  

fanIn :: (KnownNat m, KnownNat n, Monoid b) => Net m p (a -> b) -> Net (n * m) p (Vector n a -> b)
fanIn = fmap manyToOne . explode

fanIn' :: (KnownNat m, Monoid b) => (p -> a -> b) -> Net m p (Vector m a -> b)
fanIn' f = mkNet (\ws xs -> fold (f <$> ws <*> xs))

floatOut :: (KnownNat m, KnownNat n) => Net m (Vector n p) a -> Net (m * n) p a 
floatOut = mkNet . lmap separate . runNet

floatOut' :: (KnownNat m, KnownNat n) => (Vector n q -> p) -> Net m p a -> Net (m * n) q a
floatOut' f = mkNet . lmap (fmap f . separate) . runNet 

pmap :: (KnownNat n, KnownNat m) => (Vector n p -> Vector m p) -> Net m p a -> Net n p a
pmap f = mkNet . lmap f . runNet 

stdBias :: Num a => Net 1 a (a -> a)
stdBias = mkNet' (\w x -> w + x)  

stdNeuron :: (KnownNat m, Erf a) => Net (1+m) a (Vector m a -> a)
stdNeuron = (.) <$> mkNet' (\w (Sum x)-> erf $ w + x) <+> fanIn' (\w x -> Sum $ w * x)

stdLayer :: (KnownNat m, KnownNat n, KnownNat (1+m), Erf a) => Net (n*(1+m)) a (Vector m a -> Vector n a)
stdLayer = fanOut stdNeuron

--getGradient :: (KnownNat m, Num a) => (forall s. Net m (s (Kahn a)) (s (Kahn a))) -> Vector m a -> Vector m a
--getGradient net = grad (runNet net)

getGradient :: (KnownNat m, Num a) =>
                (forall s. Reifies s Tape => Net m (Reverse s a) (Reverse s a)) ->
                Vector m a -> Vector m a
getGradient net = grad (runNet net) 

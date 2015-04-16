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
import Control.Category ((>>>))

import Data.Monoid

import Data.Reflection
import Numeric.AD
import Numeric.AD.Internal.Reverse

import GHC.TypeLits

import Data.Proxy

import Prelude hiding (sum, head, foldl1, foldl, foldr)

newtype Net (n :: Nat) a b = Net{unNet :: DownStar (Vector n) a b}
                           deriving(Functor, Applicative, Monad, Profunctor)

runNet :: Net n a b -> Vector n a -> b
runNet = runDownStar . unNet

mkNet :: (Vector n a -> b) -> Net n a b
mkNet = Net . DownStar

mkNet' :: (a -> b) -> Net 1 a b
mkNet' f = mkNet (f . head)

instance Distributive (Net n a) where
  distribute fs = mkNet (\ps -> (\f -> (runNet f ps)) <$> fs)

--  A version of <*> that separates the arguments
infixl 4 <+>
(<+>) :: (KnownNat m) => Net m w (a -> b) -> Net n w a -> Net (m + n) w b
f <+> x = mkNet (\v -> let (fp, xp) = split v in runNet f fp $ runNet x xp)

-- A version of >>= that separates the arguments
infixl 1 >>+
(>>+) :: (KnownNat m) => Net m w a -> (a -> Net n w b) -> Net(m + n) w b
x >>+ f = mkNet (\v -> let (xp, fp) = split v in runNet(f $ runNet x xp) fp)

-- A version of composition (.) that separates the arguments
infixr 9 <|
(<|) :: (KnownNat m) => Net m w (b -> c) -> Net n w (a -> b) -> Net (m + n) w (a -> c)
bc <| ab = (.) <$> bc <+> ab

-- A forward version of composition that separates the arguments
infixl 0 |>
(|>) :: (KnownNat m) => Net m w (a -> b) -> Net n w (b -> c) -> Net (m + n) w (a -> c)
ab |> bc = (>>>) <$> ab <+> bc

-- A version of distribute that separates the arguments
explode :: (KnownNat m, KnownNat n) => Net m p a -> Net (n * m) p (Vector n a)
explode net = mkNet (\v -> let fs = separate v in runNet <$> pure net <*> fs)

-- A version of product that separates the arguments of each sequential step
iterateNet :: forall n m a p. (KnownNat m, KnownNat n, Monoid a) => Proxy n -> Net m p a -> Net (n * m) p a
iterateNet Proxy net = fmap fold $  (explode net :: Net (n * m) p (Vector n a))

-- A version of fold that separates the arguments of each sequential step
foldNet :: forall n m a b p. (KnownNat m, KnownNat n) => (a -> b -> b) -> b -> Proxy n -> Net m p a -> Net (n * m) p b
foldNet f z Proxy net = fmap (foldr f z) $  (explode net :: Net (n * m) p (Vector n a))

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

pmap :: (Vector n p -> Vector m p) -> Net m p a -> Net n p a
pmap f = mkNet . lmap f . runNet

errorNet :: (b -> b -> c) -> Net m w (a -> b) -> Net m w ((a,b) -> c)
errorNet err = fmap (\f (x,y) -> err (f x) y)

collectErrors :: (Functor f) => (b -> b -> c) -> Net m w (a -> b) -> f (a, b) -> Net m w (f c)
collectErrors err net = let net' =  errorNet err net
                        in collect (\x -> applyNet x net')

summedError :: (Functor f, Foldable f, Num c) => (b -> b -> c) -> Net m w (a -> b) -> f (a, b) -> Net m w c
summedError err net = fmap sum . collectErrors err net

autoencodeError :: (a -> b -> c) -> Net m w (a -> b) -> Net m w (a -> c)
autoencodeError err = fmap (\f x -> err x $ f x)

autoencodeErrorSum :: (Functor f, Foldable f, Num c) => (a -> b -> c) -> Net m w (a -> b) -> Net m w (f a -> c)
autoencodeErrorSum err = fmap (\f -> getSum .foldMap (Sum . f)) . autoencodeError err

applyNet :: a -> Net m w (a -> b) -> Net m w b
applyNet x = fmap ($x)

getGradient :: (KnownNat m, Num a) =>
                (forall s. Reifies s Tape => Net m (Reverse s a) (Reverse s a)) ->
                Vector m a -> Vector m a
getGradient net = grad (runNet net)

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

module Data.Net where

import Data.Profunctor
import Data.Vector.Fixed.Size
import Data.Vector.Fixed
import Control.Applicative
import Data.Distributive
import Control.Category ((>>>))
import Data.Monoid
import Data.Foldable

import Data.Reflection
import Numeric.AD
import Numeric.AD.Internal.Reverse


import Prelude hiding (sum, head, foldl1, foldl, foldr)

newtype Net (n :: Size) a b = Net {unNet :: DownStar (Vector n) a b}
                            deriving(Functor, Applicative, Monad, Profunctor)

runNet :: Net n a b -> Vector n a -> b
runNet = runDownStar . unNet

mkNet :: (Vector n a -> b) -> Net n a b
mkNet = Net . DownStar

mkNet' :: (a -> b) -> Net (N 1) a b
mkNet' f = mkNet (f . fromSingleton)

instance Distributive (Net n a) where
  distribute fs = mkNet (\ps -> (\f -> (runNet f ps)) <$> fs)

--  A version of <*> that separates the arguments
infixl 4 <+>
(<+>) :: (Known m) => Net m w (a -> b) -> Net n w a -> Net (m + n) w b
f <+> x = mkNet (\v -> let (fp, xp) = split v in runNet f fp $ runNet x xp)

-- A version of >>= that separates the arguments
infixl 1 >>+
(>>+) :: (Known m) => Net m w a -> (a -> Net n w b) -> Net(m + n) w b
x >>+ f = mkNet (\v -> let (xp, fp) = split v in runNet(f $ runNet x xp) fp)

-- A version of composition (.) that separates the arguments
infixr 9 <|
(<|) :: (Known m) => Net m w (b -> c) -> Net n w (a -> b) -> Net (m + n) w (a -> c)
bc <| ab = (.) <$> bc <+> ab

-- A forward version of composition that separates the arguments
infixl 0 |>
(|>) :: (Known m) => Net m w (a -> b) -> Net n w (b -> c) -> Net (m + n) w (a -> c)
ab |> bc = (>>>) <$> ab <+> bc

-- A version of distribute that separates the arguments
explode :: (Known m, Known n) => Net m p a -> Net (n * m) p (Vector n a)
explode net = mkNet (\v -> let fs = separate v in runNet <$> pure net <*> fs)

explode' :: (Known m) => (p -> a -> b) -> Net m p (Vector m a -> Vector m b)
explode' f = mkNet ((<*>) . fmap f)

-- A version of product that separates the arguments of each sequential step
iterateNet :: forall n m a p. (Known m, Known n, Monoid a) => Net m p a -> Net (n * m) p a
iterateNet net = fmap fold $ explode net

-- A version of fold that separates the arguments of each sequential step
foldNet :: forall n m a b p. (Known m, Known n) => (a -> b -> b) -> b -> Net m p a -> Net (n * m) p b
foldNet f z net = fmap (foldr f z) $  explode net

oneToMany :: (Functor f) => f (a -> b) -> a -> f b
oneToMany fs x = fmap ($x) fs

fanOut :: (Known m, Known n) => Net m p (a -> b) -> Net (n * m) p (a -> Vector n b)
fanOut = fmap oneToMany . explode

fanOut' :: (Known m) => (p -> a -> b) -> Net m p (a -> Vector m b)
fanOut' f = mkNet (\ws x -> (\w -> f w x) <$> ws)

manyToOne :: (Foldable f, Applicative f, Monoid b) => f (a -> b) -> f a -> b
manyToOne fs xs = fold (fs <*> xs)

fanIn :: (Known m, Known n, Monoid b) => Net m p (a -> b) -> Net (n * m) p (Vector n a -> b)
fanIn = fmap manyToOne . explode

fanIn' :: (Known m, Monoid b) => (p -> a -> b) -> Net m p (Vector m a -> b)
fanIn' f = mkNet (\ws xs -> fold (f <$> ws <*> xs))

floatOut :: (Known m, Known n) => (Vector n q -> p) -> Net m p a -> Net (m * n) q a
floatOut f = mkNet . lmap (fmap f . separate) . runNet

floatOut' :: (Known m) => (Functor f) => (Vector m q -> f p) -> DownStar f p a -> Net m q a
floatOut' f = mkNet . lmap f . runDownStar

pmap :: (Vector n p -> Vector m p) -> Net m p a -> Net n p a
pmap f = mkNet . lmap f . runNet

errorNet :: (b -> b -> c) -> Net m w (a -> b) -> Net m w ((a,b) -> c)
errorNet err = fmap (\f (x,y) -> err (f x) y)

summedErrorNet :: (Foldable f, Num c) => (b -> b -> c) -> Net m w (a -> b) -> Net m w (f (a,b) -> c)
summedErrorNet err = fmap (\f -> getSum . foldMap (Sum . f)) . errorNet err

autoencodeError :: (a -> b -> c) -> Net m w (a -> b) -> Net m w (a -> c)
autoencodeError err = fmap (\f x -> err x $ f x)

autoencodeErrorSum :: (Functor f, Foldable f, Num c) => (a -> b -> c) -> Net m w (a -> b) -> Net m w (f a -> c)
autoencodeErrorSum err = fmap (\f -> getSum .foldMap (Sum . f)) . autoencodeError err

applyNet :: a -> Net m w (a -> b) -> Net m w b
applyNet x = fmap ($x)

weights :: Net n a (Vector n a)
weights = mkNet id

getGradient :: (Known m, Num a) =>
                (forall s. Reifies s Tape => Net m (Reverse s a) (Reverse s a)) ->
                Vector m a -> Vector m a
getGradient net = grad (runNet net)

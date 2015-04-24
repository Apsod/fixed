{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE FlexibleContexts           #-}

module Data.Net where

import Data.Profunctor
import Control.Applicative
import Data.Distributive
import Control.Category ((>>>))
import Data.Monoid hiding (Product)
import Data.Foldable
import Data.Traversable

import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Identity

import Data.Reflection
import Numeric.AD
import Numeric.AD.Internal.Reverse

import Prelude hiding (sum, head, foldl1, foldl, foldr)

newtype Net f a b = Net {unNet :: DownStar f a b}
                            deriving(Functor, Applicative, Monad, Profunctor)

type (f + g) = Product f g
type (f * g) = Compose f g

runNet :: Net f a b -> f a -> b
runNet = runDownStar . unNet

mkNet :: (f a -> b) -> Net f a b
mkNet = Net . DownStar

mkNet' :: (a -> b) -> Net Identity a b
mkNet' f = mkNet (f . runIdentity)

instance Distributive (Net n a) where
  distribute fs = mkNet (\ps -> (\f -> (runNet f ps)) <$> fs)

--  A version of <*> that separates the arguments
infixl 4 <+>
(<+>) :: Net f w (a -> b) -> Net g w a -> Net (f + g) w b
f <+> x = mkNet (\(Pair fp xp) -> runNet f fp $ runNet x xp)

-- A version of >>= that separates the arguments
infixl 1 >>+
(>>+) :: Net f w a -> (a -> Net g w b) -> Net (f + g) w b
x >>+ f = mkNet (\(Pair xp fp) -> runNet(f $ runNet x xp) fp)

-- A version of composition (.) that separates the arguments
infixr 9 <|
(<|) :: Net f w (b -> c) -> Net g w (a -> b) -> Net (f + g) w (a -> c)
bc <| ab = (.) <$> bc <+> ab
-- A forward version of composition that separates the arguments
infixl 0 |>

(|>) :: Net f w (a -> b) -> Net g w (b -> c) -> Net (f + g) w (a -> c)
ab |> bc = (>>>) <$> ab <+> bc

-- A version of distribute that separates the arguments
explode :: (Applicative g) => Net f p a -> Net (g * f) p (g a)
explode net = mkNet (\(Compose fs) -> runNet <$> pure net <*> fs)

explode' :: (Applicative f) => (p -> a -> b) -> Net f p (f a -> f b)
explode' f = mkNet ((<*>) . fmap f)

-- A version of product that separates the arguments of each sequential step
iterateNet :: (Applicative g, Foldable g, Monoid a) => Net f p a -> Net (g * f) p a
iterateNet net = fmap fold $ explode net

-- A version of fold that separates the arguments of each sequential step
foldNet :: (Applicative g, Foldable g) => (a -> b -> b) -> b -> Net f p a -> Net (g * f) p b
foldNet f z net = fmap (foldr f z) $  explode net

oneToMany :: (Functor f) => f (a -> b) -> a -> f b
oneToMany fs x = fmap ($x) fs

fanOut :: (Applicative g) => Net f p (a -> b) -> Net (g * f) p (a -> g b)
fanOut = fmap oneToMany . explode

fanOut' :: (Functor f) => (p -> a -> b) -> Net f p (a -> f b)
fanOut' f = mkNet (\ws x -> (\w -> f w x) <$> ws)

manyToOne :: (Foldable f, Applicative f, Monoid b) => f (a -> b) -> f a -> b
manyToOne fs xs = fold (fs <*> xs)

fanIn :: (Applicative g, Foldable g, Monoid b) => Net f p (a -> b) -> Net (g * f) p (g a -> b)
fanIn = fmap manyToOne . explode

fanIn' :: (Applicative f, Foldable f, Monoid b) => (p -> a -> b) -> Net f p (f a -> b)
fanIn' f = mkNet (\ws xs -> fold (f <$> ws <*> xs))

floatOut :: (Functor f) => (g a -> b) -> Net f b c -> Net (Compose f g) a c
floatOut f = mkNet . lmap (fmap f . getCompose) . runNet

pmap :: (g q -> f p) -> Net f p a -> Net g q a
pmap f = mkNet . lmap f . runNet

weights :: Net f a (f a)
weights = mkNet id

errorNet :: (b -> b -> c) -> Net f w (a -> b) -> Net f w ((a,b) -> c)
errorNet err = fmap (\f (x,y) -> err (f x) y)

summedErrorNet :: (Foldable f, Num c) => (b -> b -> c) -> Net m w (a -> b) -> Net m w (f (a,b) -> c)
summedErrorNet err = fmap (\f -> getSum . foldMap (Sum . f)) . errorNet err

autoencodeError :: (a -> b -> c) -> Net m w (a -> b) -> Net m w (a -> c)
autoencodeError err = fmap (\f x -> err x $ f x)

autoencodeErrorSum :: (Functor f, Foldable f, Num c) => (a -> b -> c) -> Net m w (a -> b) -> Net m w (f a -> c)
autoencodeErrorSum err = fmap (\f -> getSum . foldMap (Sum . f)) . autoencodeError err

applyNet :: a -> Net m w (a -> b) -> Net m w b
applyNet x = fmap ($x)

getGradient :: (Traversable f, Num a) =>
                (forall s. Reifies s Tape => Net f (Reverse s a) (Reverse s a)) ->
                f a -> f a
getGradient net = grad (runNet net)

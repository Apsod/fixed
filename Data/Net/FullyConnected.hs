{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}

module Data.Net.FullyConnected where

import Control.Applicative
import Data.Net
import Data.Vector.Fixed.Linear
import Data.Vector.Fixed
import Data.Vector.Fixed.Size (Known)

import Data.Profunctor
import Data.Functor.Identity
import Data.Foldable

import Prelude hiding (sum)

sumNeuron :: (Known c, Num a) => Net (Vector c) a (Vector c a -> a)
sumNeuron = mkNet dot

sumNeuron' :: (Applicative f, Foldable f, Num a) => Net f a (f a -> a)
sumNeuron' = mkNet (\w -> foldl' (+) 0 . liftA2 (*) w)

neuronBias :: (Num a) => Net Identity a (a -> a)
neuronBias = mkNet' (+)

sumLayer :: (Known r, Known c, Num a) => Net (Matrix r c) a (Vector c a -> Vector r a)
sumLayer = mkNet (\m -> mXv m)

sumLayer' :: (Applicative f, Applicative g, Foldable f, Num a) => Net (g * f) a (f a -> g a)
sumLayer' = fanOut sumNeuron'

layerBias :: (Known r, Num a) => Net (Vector r) a (Vector r a -> Vector r a)
layerBias = mkNet add

layerBias' :: (Applicative f, Num a) => Net f a (f a -> f a)
layerBias' = mkNet (liftA2 (+))

fullyConnectedLayer :: (Known r, Known c, Num a) => (a -> b) -> Net (Vector r + Matrix r c) a (Vector c a -> Vector r b)
fullyConnectedLayer sigmoid = rmap (fmap sigmoid) <$>
                              layerBias <|
                              sumLayer

fullyConnectedLayer' :: (Applicative f, Applicative g, Foldable f, Num a) => (a -> b) -> Net (g + (g * f)) a (f a -> g b)
fullyConnectedLayer' sigmoid = rmap (fmap sigmoid) <$>
                               layerBias' <|
                               sumLayer'

fullyConnectedNeuron :: (Known c, Num a) => (a -> b) -> Net (Identity + Vector c) a (Vector c a -> b)
fullyConnectedNeuron sigmoid = rmap sigmoid <$>
                               neuronBias <|
                               sumNeuron

fullyConnectedNeuron' :: (Applicative f, Foldable f, Num a) => (a -> b) -> Net (Identity + f) a (f a -> b)
fullyConnectedNeuron' sigmoid = rmap sigmoid <$>
                                neuronBias <|
                                sumNeuron'


{-
Note that (fullyConnectedLayer) is isomorphic to (fanOut fullyConnectedNeuron)
-}

{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE OverloadedLists     #-}

module Data.Net.Convolutional where

import Control.Applicative

import Data.Net
import Data.Net.FullyConnected
import Data.Net.Recurrent

import Data.Vector.Fixed.Linear
import Data.Vector.Fixed as Fixed
import qualified Data.Vector.Fixed.Size as Size
import Data.Vector.Fixed.Size (S, Known, getInt, Size(..))
import Data.Vector.Fixed.Indexed
import Control.Monad.State.Strict

import Data.Number.Erf

import Data.Maybe
import Data.Distributive
import Control.Comonad
import Data.Foldable

import Data.Functor.Product
import Data.Functor.Compose

import Control.Arrow ((***))

import Data.Proxy

import Prelude hiding (sum)

type Convolution f a b = f a -> b

type Odd a = S (a Size.+ a)
type Square n = (Odd n) Size.* (Odd n)

square :: forall n. (Known n) => Matrix (Odd n) (Odd n) (Int, Int)
square = let width = getInt (Proxy :: Proxy n)
         in (\(i,j) -> (i - width, j - width)) <$> indices

squareExperiment :: (Known r, Known c, Known n) => Indexed (Matrix r c) a -> Matrix (Odd n) (Odd n) (Maybe a)
squareExperiment = experiment (\(i,j) -> ((+i) *** (+j)) <$> square)

convolutionNet :: (Indexable f) => Net g p (Indexed f a -> b) -> Net g p (f a -> f b)
convolutionNet = fmap convolution

squareConv :: (Known r, Known c, Known n, Erf a, Foldable f, Applicative f, Applicative g) => Net
              (g + (g*(Matrix (Odd n) (Odd n) * f)))
              a
              (Matrix r c (f a) -> Matrix r c (g a))
squareConv = convolutionNet $ (\f -> f . Compose . fmap (fromMaybe (pure 0)) . squareExperiment) <$> fullyConnectedLayer' erf

gunzip :: (Functor f) =>  f (a,b) -> (f a, f b)
gunzip xs = (fmap fst xs, fmap snd xs)

floatState :: (Applicative f) => f (State a b) -> State (f a) (f b)
floatState = state . fmap gunzip . (<*>) . fmap runState


{-
type Norm f a = f a -> a

distNeuron :: (Known c, Num a) => Norm (Vector c) a -> Net c a (Vector c a -> a)
distNeuron norm = mkNet (\v -> norm . sub v)

distLayer :: (Known r, Known c, Num a) => Norm (Vector c) a -> Net (r*c) a (Vector c a -> Vector r a)
distLayer norm = mkNet (\ms v -> onRows (norm . sub v) (Matrix ms))

normalizedGaussianRBF :: (Known r, Known c, Floating a) => Net (r + r*c) a (Vector c a -> Vector r a)
normalizedGaussianRBF = (\betas dist v ->  normalize1 $ (\b d -> exp (b * d)) <$> betas <*> dist v)
                        <$> mkNet (fmap (negate . abs))
                        <+> distLayer norm2Square
-}

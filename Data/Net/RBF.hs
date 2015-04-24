{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}

module Data.Net.RBF where

import Control.Applicative
import Data.Net
import Data.Vector.Fixed.Linear
import Data.Vector.Fixed
import Data.Functor.Compose
import Data.Foldable

import Prelude hiding (sum)

type Norm f a = f a -> a

distNeuron :: (Applicative f, Num a) => Norm f a -> Net f a (f a -> a)
distNeuron norm = mkNet (\v -> norm . liftA2 (-) v)

distLayer :: (Applicative f, Applicative g, Num a) => Norm f a -> Net (g*f) a (f a -> g a)
distLayer norm = mkNet (\(Compose centers) v -> fmap (norm . liftA2 (-) v) centers)

normalizedGaussianRBF :: (Foldable f, Foldable g, Applicative g, Applicative f, Floating a) => Net (f + (f*g)) a (g a -> f a)
normalizedGaussianRBF = (\betas dist v ->  normalize1 $ (\b d -> exp (b * d)) <$> betas <*> dist v)
                        <$> mkNet (fmap (negate . abs))
                        <+> distLayer norm2Square
  where norm2Square  = foldl' (\acc x -> acc + (x^2)) 0
        normalize1 x = let s = foldl' (+) 0 x
                       in fmap (/s) x 
{-
normalizedGaussianRBF' :: (Known r, Known c, Floating a) => Net (r + r*c) a (Vector c a -> Vector r a)
normalizedGaussianRBF' = (\betas dist v ->  normalize1 $ (\b d -> exp (b * d)) <$> betas <*> dist v)
                         <$> mkNet (fmap (negate . abs))
                         <+> distLayer norm2Square
-}

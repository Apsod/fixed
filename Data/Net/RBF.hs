{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}

module Data.Net.FullyConnected where

import Control.Applicative 
import Data.Net 
import Data.Vector.Fixed.Linear 
import Data.Vector.Fixed
import Data.Vector.Fixed.Size 

import Prelude hiding (sum) 

distNeuron :: (Known c, Num a) => Net c a (Vector c a -> a)  
distNeuron = mkNet dist2Square

distLayer :: (Known r, Known c, Num a) => (Vector c a -> Vector c a -> a) -> Net (r*c) a (Vector c a -> Vector r a)
distLayer dist = mkNet (\ms v -> onRows (dist v) (Matrix ms))


normalizedGaussianRBF :: (Known r, Known c, Floating a) => Net (r + r*c) a (Vector c a -> Vector r a)
normalizedGaussianRBF = (\betas dist v ->  normalize1 $ (\b d -> exp (b * d)) <$> betas <*> dist v) 
                        <$> mkNet (fmap (negate . abs)) 
                        <+> distLayer dist2Square 

{-
neuronBias :: (Num a) => Net (N 1) a (a -> a)
neuronBias = mkNet' (+) 

sumLayer :: (Known r, Known c, Num a) => Net (r * c) a (Vector c a -> Vector r a)
sumLayer = mkNet (\m -> mXv (Matrix m))

layerBias :: (Known r, Num a) => Net r a (Vector r a -> Vector r a)
layerBias = mkNet add

fullyConnectedLayer :: (Known r, Known c, Num a) => (a -> b) -> Net (r + r * c) a (Vector c a -> Vector r b) 
fullyConnectedLayer sigmoid = (\bias sum -> fmap sigmoid . bias . sum)
                              <$> layerBias
                              <+> sumLayer

fullyConnectedNeuron :: (Known c, Num a) => (a -> b) -> Net (S c) a (Vector c a -> b)  
fullyConnectedNeuron sigmoid = (\bias sum -> sigmoid . bias . sum)
                               <$> neuronBias 
                               <+> sumNeuron
-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}

module Data.Net.FullyConnected where

import Control.Applicative 
import Data.Net 
import Data.Vector.Fixed.Linear 
import Data.Vector.Fixed
import Data.Vector.Fixed.Size 

import Prelude hiding (sum) 

sumNeuron :: (Known c, Num a) => Net c a (Vector c a -> a)  
sumNeuron = mkNet dot 

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

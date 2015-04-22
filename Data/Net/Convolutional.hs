{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}

module Data.Net.Convolutional where

import Control.Applicative 
import Data.Net 
import Data.Vector.Fixed.Linear 
import Data.Vector.Fixed
import Data.Vector.Fixed.Size
import Data.Distributive
import Control.Comonad

import Prelude hiding (sum)

type Convolution f a b = f a -> b

sweep :: (Comonad f) => Net m w (f a -> b) -> Net m w (f a -> f b) 
sweep = fmap extend 

--pooling :: (Comonad f, Distributive g) => (f a -> g b) -> ( -> c) -> f a -> g c
pooling conv = distribute . (extend conv) 

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

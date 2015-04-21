{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}

module Data.Vector.Fixed.Indexed
       (Indexable(..)
       ,Indexed
       ,experiment
       ,convolution
       ) where

import Control.Comonad 
import Data.Maybe 

class (Functor f) => Indexable f where
  type Index f :: * 
  indices      :: f (Index f) 
  (!?)         :: f a -> (Index f) -> Maybe a

-- (\x  -> fmap (\ix -> x !? ix) indices) == fmap Just 
-- (\ix -> indices !? ix) == Just 

data Indexed f a = Indexed (Index f) (f a)

instance (Functor f) => Functor (Indexed f) where
  fmap f (Indexed i x) = Indexed i (fmap f x)

instance (Indexable f) => Comonad (Indexed f) where
  extract (Indexed i x) = fromJust (x !? i) 
  extend f (Indexed i x) = Indexed i ((\j -> f $ Indexed j x) <$> indices)

fakeIndex :: f a -> Indexed f a 
fakeIndex = Indexed undefined 

forgetIndex :: Indexed f a -> f a
forgetIndex (Indexed _ x) = x

experiment :: (Functor g, Indexable f) => (Index f -> g (Index f)) -> Indexed f a -> g (Maybe a)
experiment f (Indexed i x) = fmap (x!?) (f i)  

convolution :: (Indexable f) => (Indexed f a -> b) -> f a -> f b
convolution f = forgetIndex . extend f . fakeIndex 

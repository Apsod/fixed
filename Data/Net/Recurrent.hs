{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds       #-}

module Data.Net.Recurrent where

import Control.Applicative
import Data.Net
import Data.Net.FullyConnected
import Data.Vector.Fixed
import Data.Vector.Fixed.Size
import Control.Monad.State

import Prelude hiding (sum)

lstm' :: (Num a) => Net (Vector (N 2)) a (Vector (N 4) a -> State a a)
lstm' = (\summer [x, g1, g2, g3] ->
          let p1 = x * g1
          in do
            p2 <- (g2*) <$> get
            put (summer [p1, p2])
            (g3*) <$> get)
        <$> sumNeuron

unfoldM :: (Functor m, Monad m) => (a -> m a) -> Int -> a -> m [a]
unfoldM f = go
  where go 0 _ = return []
        go n x = do
          x' <- f x
          (x:) <$> go (n-1) x'

unfoldForever :: (Functor m, Monad m) => (a -> m a) -> a -> m [a]
unfoldForever f = go
  where go x = do
          x' <- f x
          (x:) <$> go x'

unrollState :: (a -> State s a) -> a -> State s [(a, s)]
unrollState f = go
  where go x = do
          t  <- (,) <$> f x <*> get
          (t:) <$> go (fst t)

unrollStateN :: (a -> State s a) -> Int -> a -> State s [(a, s)]
unrollStateN f = go 
  where go 0 _ = return []
        go n x = do
          t <- (,) <$> f x <*> get
          (t:) <$> go (n-1) (fst t) 

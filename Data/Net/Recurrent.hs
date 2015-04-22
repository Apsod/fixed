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

lstm' :: (Num a) => Net (N 2) a (Vector (N 4) a -> State a a)
lstm' = (\summer [x, g1, g2, g3] ->
          let p1 = x * g1
          in do
            p2 <- (g2*) <$> get
            put (summer [p1, p2])
            (g3*) <$> get)
        <$> sumNeuron

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
import Data.Vector.Fixed.Size
import Data.Vector.Fixed.Indexed
import Control.Monad.State.Strict

import Data.Number.Erf

import Data.Maybe
import Data.Distributive
import Control.Comonad

import Control.Arrow ((***))

import Data.Proxy

import Prelude hiding (sum)

type Convolution f a b = f a -> b

type Odd a = S (a + a)
type Square n = (Odd n) * (Odd n)

square :: forall n. (Known n) => Matrix (Odd n) (Odd n) (Int, Int)
square = let width = getInt (Proxy :: Proxy n)
         in (\(i,j) -> (i - width, j - width)) <$> indices

cross :: Vector (N 4) (Int, Int)
cross = [( 0,  1)
        ,( 0, -1)
        ,( 1,  0)
        ,(-1,  0)]

squareExperiment :: (Known r, Known c, Known n) => Indexed (Matrix r c) a -> Matrix (Odd n) (Odd n) (Maybe a)
squareExperiment = experiment (\(i,j) -> ((+i) *** (+j)) <$> square)


interfaceExperiment :: (Known r, Known c) => Indexed (Matrix r c) (Vector (N 4) a) -> Vector (N 4) (Maybe a)
interfaceExperiment = imap (\ix -> fmap (Fixed.! ix)) . experiment (\(i,j) -> ((+i) *** (+j)) <$> cross)


convolutionalLSTM :: (b -> Vector (N 4) a -> Vector (N 4) b)
                  -> (b -> b -> b)
                  -> (b -> b -> b)
                  -> Vector (N 4) a -> State b b
convolutionalLSTM gatingFunctions product sum v = do
  s <- get
  let [x, g1,g2,g3] = gatingFunctions s v
      p1 = product x  g1
      p2 = product s  g2
  put (sum p1 p2)
  product g3 <$> get

interfaceLSTM :: (Known r, Known c)
                 => b
                 -> (Vector (N 4) b -> a -> (Vector (N 4) b))
                 -> (a -> Vector (N 4) b -> Vector (N 4) a)
                 -> (a -> a -> a)
                 -> (a -> a -> a)
                 -> Indexed (Matrix r c) (Vector (N 4) b)
                 -> State a (Vector (N 4) b)
interfaceLSTM def outputFunctions gatingFunctions product sum = (\v -> fmap (outputFunctions v) $ convolutionalLSTM gatingFunctions product sum v) . fmap (fromMaybe def) . interfaceExperiment

test :: (Known r, Known c)
        => b
        -> (Vector (N 4) b -> a -> Vector (N 4) b)
        -> (Vector (N 4) b -> a -> Vector (N 4) a)
        -> (a -> a -> a)
        -> (a -> a -> a)
        -> Matrix r c (Vector (N 4) b)
        -> State (Matrix r c a) (Matrix r c (Vector (N 4) b))
test def outputFunctions gatingFunctions product sum = let c = convolution $
                                                               interfaceLSTM def outputFunctions (flip gatingFunctions) product sum
                                                       in floatState . c

{-
gatingNet :: (Known m, Known n, Num a) =>
             Net (
               -}
gatingNet :: (Known m, Known n, Erf a)
             => Net
             ((N 4 * m) + (N 4 * m) * (m + (N 4 * n))) a
             (Vector (N 4) (Vector n a) -> Vector m a -> Vector (N 4) (Vector m a))
gatingNet = (\f i -> separate . f . (Fixed.++ (flatten i))) <$> fullyConnectedLayer erf

outputNet :: (Known m, Known n, Erf a) =>
             Net ((N 4 * n) + (N 4 * n) * (m + (N 4 * n)))
             a
             (Vector (N 4) (Vector n a) -> Vector m a -> Vector (N 4) (Vector n a))
outputNet = (\f i -> separate . f . (Fixed.++ (flatten i))) <$> fullyConnectedLayer erf


tot :: (Known r, Known c, Known m, Known n, Erf a) =>
       Net (((N 4 * n) + (N 4 * n) * (m + (N 4 * n))) +
            ((N 4 * m) + (N 4 * m) * (m + (N 4 * n))) +
            (m + m))
       a
       (Matrix r c (Vector (N 4) (Vector n a)) -> State (Matrix r c (Vector m a)) (Matrix r c (Vector (N 4) (Vector n a))))
tot = (\out gate sum -> test (pure 0) out gate (liftA2 (*)) sum)
      <$> outputNet
      <+> gatingNet
      <+> weightedSumNet

scaleNet :: (Known m, Num a) =>
            Net m a (Vector m a -> Vector m a)
scaleNet = mkNet (liftA2 (*))

weightedSumNet :: (Known m, Num a) =>
                  Net (m + m) a (Vector m a -> Vector m a -> Vector m a)
weightedSumNet = (\s1 s2 v w -> liftA2 (+) (s1 v) (s2 w)) <$> scaleNet <+> scaleNet

gunzip :: (Functor f) =>  f (a,b) -> (f a, f b)
gunzip xs = (fmap fst xs, fmap snd xs)

floatState :: (Applicative f) => f (State a b) -> State (f a) (f b)
floatState = state . fmap gunzip . (<*>) . fmap runState

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

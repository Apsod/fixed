{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE Rank2Types          #-}

module Main where

import Data.Binary
import qualified Data.ByteString.Lazy as Lazy
import Data.Binary.Get

import Control.Applicative
import Control.Comonad
import Control.Monad as Monad

import Data.Vector.Fixed.Linear
import Data.Vector.Fixed hiding ((!))
import Data.Vector.Fixed.Size (Size(..)) 

import Data.Net
import Data.Net.FullyConnected
import Data.Net.Convolutional

import System.Environment

import Control.Monad.ST
import System.Random.MWC
import System.Random.MWC.Distributions

import Data.Traversable
import Data.Foldable

import Data.Number.Erf

import Prelude hiding(sum, foldl1)

fromWord :: (Floating a) => Word8 -> a
fromWord = (/255) . fromIntegral

oneHot :: (Num a) => Word8 -> Vector (N 10) a
oneHot n = generate (\ix -> if fromIntegral n == ix then 1 else 0)

iterateN :: (a -> a) -> Int -> (a -> a)
iterateN f = go
  where go 0 = id
        go n = go (n-1) . f 

type ParameterSpace = ((Vector (N 20) + (Vector (N 20) * (Matrix (Odd (N 1)) (Odd (N 1))* Vector (N 20)))) +
                       (Vector (N 10) + (Vector (N 10) * Vector (N 20))))

main :: IO ()
main = do
  [tp, lp] <- getArgs
  images <- imageFromFile tp
  labels <- labelFromFile lp
  p0 <- runIO (generateRandom (Random standard)) :: IO (ParameterSpace Double)
  let n :: (Erf a) => Net
           ParameterSpace
           a
           a
      n = applyNet (Prelude.take 2 $ zipWith (\i l -> (i, oneHot l)) images labels) (summedErrorNet dist1 $ (\f g -> g . (!(14,14)) . (iterateN f 10) . fmap (pure .fromWord)) <$> squareConv <+> fullyConnectedLayer' erf)
      gradient = getGradient n
  print . Data.Foldable.toList $ gradient p0

data Type = Unsigned
          | Signed
          | Int2
          | Int4
          | Float4
          | Float8
            deriving(Show)

data IDX = IDX {getType :: Type, getDims :: [Int]}
         deriving(Show)

toInt :: (Integral a) => a -> Int
toInt = fromIntegral

getIDX :: Get IDX
getIDX = do
  0    <- getWord8
  0    <- getWord8
  0x08 <- getWord8
  d <- toInt <$> getWord8
  ds <- Monad.replicateM d $ toInt <$> (get :: Get Word32)
  return $ IDX Unsigned ds

imageFromFile :: FilePath -> IO [Matrix (N 28) (N 28) Word8]
imageFromFile fp = runGet getImageData <$> Lazy.readFile fp

labelFromFile :: FilePath -> IO [Word8]
labelFromFile fp = runGet getLabelData <$> Lazy.readFile fp

getImageData :: Get [Matrix (N 28) (N 28) Word8]
getImageData = do
  IDX Unsigned [numImages, 28, 28] <- getIDX
  Monad.replicateM numImages (Matrix . fromList <$> (Monad.replicateM (28*28) getWord8))

getLabelData :: Get [Word8]
getLabelData = do
  IDX Unsigned [numImages] <- getIDX
  Monad.replicateM numImages getWord8

newtype Random a = Random{runRandom :: forall s. Gen s -> ST s a}

instance Functor Random where
  fmap f (Random r) = Random (\g -> f <$> r g)

instance Applicative Random where
  pure x = Random (const $ pure x)
  (Random rf) <*> (Random rx) = Random (\g -> rf g <*> rx g)

instance Monad Random where
  return = pure
  (Random rx) >>= f = Random (\g -> rx g >>= (\x -> runRandom (f x) g))

runOn :: Random a -> (forall s. Gen s) -> a
runOn rx g = runST (runRandom rx g)

runIO :: Random a -> IO a
runIO = withSystemRandom . runRandom

generateRandom :: (Applicative f, Traversable f) => Random a -> Random (f a)
generateRandom r = sequenceA $ pure r

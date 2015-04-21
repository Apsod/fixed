{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable          #-}

module Data.Vector.Fixed
       (Vector
       ,sizeAgnostic
       ,forgetSize
       ,empty
       ,getSize
       ,generate, generateM
       ,iterate
       ,replicateM
       ,(!), (!?)
       ,tail, head, uncons
       ,init, last, unsnoc
       ,split, (++)
       ,separate, flatten
       ,backpermute
       ,imap
       ,reverse
       ,fromList
       ,toList
       ) where

import GHC.Exts (IsList, fromList, toList, Item)

import Data.Proxy (Proxy(..))

import qualified Data.Vector as Vector

import Control.DeepSeq (NFData)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Control.Applicative (Applicative, (<$>), pure, (<*>))
import Data.Distributive (Distributive, distribute)

import Data.Vector.Fixed.Size 

import Prelude hiding (iterate, tail, head, init, last, (++), reverse)
import Control.Arrow ((***),(&&&))

newtype Vector (n :: Size) a = Vector {forgetSize :: Vector.Vector a}
                            deriving(Show, Eq, Ord, Functor, Foldable, Traversable, NFData)

forgetful :: (Vector.Vector a -> Vector.Vector b) -> Vector n a -> Vector m b
forgetful f = Vector . sizeAgnostic f

sizeAgnostic :: (Vector.Vector a -> b) -> Vector n a -> b
sizeAgnostic f = f . forgetSize

unsafeIndex :: Vector n a -> Int -> a
unsafeIndex = sizeAgnostic Vector.unsafeIndex

generate :: forall n. (Known n) => forall a. (Int -> a) -> Vector n a
generate = Vector . Vector.generate (getInt (Proxy :: Proxy n))

generateM :: forall m n. (Functor m, Monad m, Known n) => forall a. (Int -> m a) -> m (Vector n a)
generateM = fmap Vector . Vector.generateM (getInt (Proxy :: Proxy n)) 

instance (Known n) => Applicative (Vector n) where
  pure = Vector . Vector.replicate (getInt (Proxy :: Proxy n))
  (<*>) (Vector fs) = forgetful (Vector.zipWith ($) fs)

instance (Known n) => Distributive (Vector n) where
  distribute xs = generate (\ix -> (flip unsafeIndex ix) <$> xs)

instance (Known n) => Monad (Vector n) where
  return = pure
  v >>= f = let v' = f <$> v
            in generate (\ix -> unsafeIndex (unsafeIndex v' ix) ix)

instance (Known n) => IsList (Vector n a) where
  type Item (Vector n a) = a
  toList   = Vector.toList . forgetSize
  fromList = let nVal = getInt (Proxy :: Proxy n)
             in Vector . Vector.fromList . (\xs -> if hasLength nVal xs
                                                   then xs
                                                   else error $ Prelude.concat
                                                        ["length of list does not match size of vector ",
                                                         "(", show nVal, ")",
                                                         " in Data.Vector.Fixed.fromList"])
    where  hasLength :: Int -> [a] -> Bool
           hasLength n xs = let (prefix, suffix) = Prelude.splitAt n xs
                            in null suffix && length prefix == n

empty :: Vector (N 0) a
empty = Vector $ Vector.empty

getSize :: (Known n) => Vector n a -> Int
getSize (_ :: Vector n a)  = getInt (Proxy :: Proxy n)

iterate :: forall n. (Known n) => forall a. (a -> a) -> a -> Vector n a
iterate f = Vector . Vector.iterateN (getInt (Proxy :: Proxy n)) f

replicateM :: forall m n. (Known n, Functor m, Monad m) => forall a. m a -> m (Vector n a)
replicateM = fmap Vector . Vector.replicateM (getInt (Proxy :: Proxy n))

(!) :: Vector n a -> Int -> a
v ! ix = (Vector.!) (forgetSize v) ix

(!?) :: (Known n) => Vector n a -> Int -> Maybe a
v !? ix = if 0 <= ix && ix < getSize v
          then Just $ unsafeIndex v ix
          else Nothing

tail :: Vector (S n) a -> Vector n a
tail = forgetful Vector.unsafeTail

head :: Vector (S n) a -> a
head = sizeAgnostic Vector.unsafeHead

uncons :: Vector (S n) a -> (a, Vector n a)
uncons = (head &&& tail)

init :: Vector (S n) a -> Vector n a
init = forgetful Vector.unsafeInit

last :: Vector (S n) a -> a
last = sizeAgnostic Vector.unsafeLast

unsnoc :: Vector (S n) a -> (Vector n a, a)
unsnoc = (init &&& last)

(++) :: Vector n a -> Vector m a -> Vector (n + m) a
(++) (Vector v) = forgetful (v Vector.++)

split :: forall n. (Known n) => forall a m. Vector (n + m) a -> (Vector n a, Vector m a)
split (Vector v) = let nVal = getInt (Proxy :: Proxy n)
                   in  (Vector *** Vector) $ Vector.splitAt nVal v

separate :: forall n m. (Known n, Known m) => forall a. Vector (n * m) a -> Vector n (Vector m a)
separate (Vector v) = let mVal  = getInt (Proxy :: Proxy m)
                          chunk ix = Vector $ Vector.unsafeSlice (ix * mVal) mVal v
                      in generate chunk

flatten :: forall n m. (Known n, Known m) => forall a. Vector n (Vector m a) -> Vector (n * m) a
flatten v = let cols = getInt (Proxy :: Proxy m) 
            in generate (\ix -> let (rix, cix) = quotRem ix cols
                                in v ! rix ! cix)

backpermute :: Vector n a -> Vector m Int -> Vector m a
backpermute (Vector v) = forgetful (Vector.backpermute v)

imap :: (Int -> a -> b) -> Vector n a -> Vector n b
imap f = forgetful (Vector.imap f)

reverse :: Vector n a -> Vector n a
reverse = forgetful Vector.reverse

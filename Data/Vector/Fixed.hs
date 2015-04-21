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
       ,singleton
       ,intNat
       ,getSize
       ,inBounds 
       ,generate
       ,iterate
       ,replicateM
       ,generateM
       ,(!)
       ,(!?)
       ,tail
       ,head
       ,uncons
       ,init
       ,last
       ,unsnoc
       ,takeTo
       ,dropTo
       ,split
       ,(++)
       ,separate
       ,flatten
       ,backpermute
       ,imap
       ,reverse
       ,fromList
       ,toList
       ) where

import GHC.Exts (IsList, fromList, toList, Item)
import GHC.TypeLits
import Data.Proxy (Proxy(..))

import qualified Data.Vector as Vector

import Control.DeepSeq (NFData)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Control.Applicative (Applicative, (<$>), pure, (<*>))
import Data.Distributive (Distributive, distribute)

import Prelude hiding (iterate, tail, head, init, last, (++), reverse)
import Control.Arrow ((***),(&&&))
import Data.Indexed 


newtype Vector (n :: Nat) a = Vector {forgetSize :: Vector.Vector a}
                            deriving(Show, Eq, Ord, Functor, Foldable, Traversable, NFData)

instance (KnownNat n) => Applicative (Vector n) where
  pure = Vector . Vector.replicate (intNat (Proxy :: Proxy n))
  (<*>) (Vector fs) = forgetful (Vector.zipWith ($) fs)

instance (KnownNat n) => Distributive (Vector n) where
  distribute xs = generate (\ix -> (flip unsafeIndex ix) <$> xs)

instance (KnownNat n) => Monad (Vector n) where
  return = pure
  v >>= f = let v' = f <$> v
            in generate (\ix -> unsafeIndex (unsafeIndex v' ix) ix)

instance (KnownNat n) => IsList (Vector n a) where
  type Item (Vector n a) = a
  toList   = Vector.toList . forgetSize
  fromList = let nVal = intNat (Proxy :: Proxy n)
             in Vector . Vector.fromList . (\xs -> if hasLength nVal xs
                                                   then xs
                                                   else error $ Prelude.concat
                                                        ["length of list does not match size of vector ",
                                                         "(", show nVal, ")",
                                                         " in Data.Vector.Fixed.fromList"])

instance (KnownNat n) => Indexable (Vector n) where
  type Index (Vector n) = Int
  indices = generate id
  v !? ix = if 0 <= ix && ix < getSize v
            then Just $ unsafeIndex v ix
            else Nothing
  
  


sizeAgnostic :: (Vector.Vector a -> b) -> Vector n a -> b
sizeAgnostic f = f . forgetSize

forgetful :: (Vector.Vector a -> Vector.Vector b) -> Vector n a -> Vector m b
forgetful f = Vector . sizeAgnostic f

hasLength :: Int -> [a] -> Bool
hasLength n xs = let (prefix, suffix) = Prelude.splitAt n xs
                 in null suffix && length prefix == n

intNat :: (KnownNat n) => Proxy n -> Int
intNat = fromIntegral . natVal

empty :: Vector 0 a
empty = Vector $ Vector.empty

singleton :: a -> Vector 1 a
singleton = Vector . Vector.singleton

getSize :: (KnownNat n) => Vector n a -> Int
getSize (_ :: Vector n a)  = intNat (Proxy :: Proxy n)

inBounds :: (KnownNat n) => Vector n a -> Int -> Bool
inBounds v = (< getSize v)   

generate :: forall n. (KnownNat n) => forall a. (Int -> a) -> Vector n a
generate = Vector . Vector.generate (intNat (Proxy :: Proxy n))

iterate :: forall n. (KnownNat n) => forall a. (a -> a) -> a -> Vector n a
iterate f = Vector . Vector.iterateN (intNat (Proxy :: Proxy n)) f

replicateM :: forall m n. (KnownNat n, Monad m, Functor m) => forall a. m a -> m (Vector n a)
replicateM x = Vector <$> Vector.replicateM (intNat (Proxy :: Proxy n)) x

generateM :: forall m n. (KnownNat n, Monad m , Functor m) => forall a. (Int -> m a) -> m (Vector n a)
generateM f = Vector <$> Vector.generateM (intNat (Proxy :: Proxy n)) f

unsafeIndex :: Vector n a -> Int -> a
unsafeIndex = sizeAgnostic Vector.unsafeIndex

(!) :: Vector n a -> Int -> a
v ! ix = (Vector.!) (forgetSize v) ix

{-
(!?) :: (KnownNat n) => Vector n a -> Int -> Maybe a
v !? ix = if 0 <= ix && ix < getSize v
          then Just $ unsafeIndex v ix
          else Nothing
-}

tail :: Vector (1+n) a -> Vector n a
tail = forgetful Vector.unsafeTail

head :: Vector (1+n) a -> a
head = sizeAgnostic Vector.unsafeHead

uncons :: Vector (1+n) a -> (a, Vector n a)
uncons = (head &&& tail)

init :: Vector (n+1) a -> Vector n a
init = forgetful Vector.unsafeInit

last :: Vector (n+1) a -> a
last = sizeAgnostic Vector.unsafeLast

unsnoc :: Vector (n+1) a -> (Vector n a, a)
unsnoc = (init &&& last)

takeTo :: forall m n. (KnownNat n, KnownNat m,  m <= n) => forall a. Vector n a -> Vector m a
takeTo = let mVal = intNat (Proxy :: Proxy m)
         in forgetful (Vector.unsafeTake mVal)

dropTo :: forall m n. (KnownNat n, KnownNat m, m <= n) => forall a. Vector n a -> Vector m a
dropTo = let mVal = intNat (Proxy :: Proxy m)
             nVal = intNat (Proxy :: Proxy n)
         in forgetful (Vector.unsafeDrop (nVal - mVal))

(++) :: Vector n a -> Vector m a -> Vector (n + m) a
(++) (Vector v) = forgetful (v Vector.++)

split :: forall n. (KnownNat n) => forall a m. Vector (n + m) a -> (Vector n a, Vector m a)
split (Vector v) = let nVal = intNat (Proxy :: Proxy n)
                   in  (Vector *** Vector) $ Vector.splitAt nVal v

separate :: forall n m. (KnownNat n, KnownNat m) => forall a. Vector (n * m) a -> Vector n (Vector m a)
separate (Vector v) = let mVal  = intNat (Proxy :: Proxy m)
                          chunk ix = Vector $ Vector.unsafeSlice (ix * mVal) mVal v
                      in generate chunk

flatten :: Vector n (Vector m a) -> Vector (n * m) a
flatten = forgetful (Vector.concatMap forgetSize)

backpermute :: Vector n a -> Vector m Int -> Vector m a
backpermute (Vector v) = forgetful (Vector.backpermute v)

imap :: (Int -> a -> b) -> Vector n a -> Vector n b
imap f = forgetful (Vector.imap f)

reverse :: Vector n a -> Vector n a
reverse = forgetful Vector.reverse

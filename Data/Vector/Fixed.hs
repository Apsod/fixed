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
       ,empty
       ,getSize
       ,generate
       ,iterate
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
       ,separate
       ) where

import GHC.Exts (IsList, fromList, toList, Item)
import GHC.TypeLits
import Data.Proxy (Proxy(..))

import qualified Data.Vector as Vector

import Control.DeepSeq
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Control.Applicative (Applicative, (<$>), pure, (<*>))
import Data.Distributive (Distributive, distribute)

import Prelude hiding (iterate, tail, head, init, last)
import Control.Arrow ((***),(&&&))

newtype Vector (n :: Nat) a = Vector {forgetSize :: Vector.Vector a}
                            deriving(Show, Eq, Ord, Functor, Foldable, Traversable, NFData)

instance (KnownNat n) => Applicative (Vector n) where
  pure = Vector . Vector.replicate (intNat (Proxy :: Proxy n))
  Vector fs <*> Vector xs = Vector $ Vector.zipWith ($) fs xs

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



hasLength :: Int -> [a] -> Bool
hasLength n xs = let (prefix, suffix) = Prelude.splitAt n xs
                 in null suffix && length prefix == n

intNat :: (KnownNat n) => Proxy n -> Int
intNat = fromIntegral . natVal

empty :: Vector 0 a
empty = Vector $ Vector.empty

getSize :: (KnownNat n) => Vector n a -> Int
getSize (_ :: Vector n a)  = intNat (Proxy :: Proxy n)

generate :: forall n. (KnownNat n) => forall a. (Int -> a) -> Vector n a
generate = Vector . Vector.generate (intNat (Proxy :: Proxy n))

iterate :: forall n. (KnownNat n) => forall a. (a -> a) -> a -> Vector n a
iterate f = Vector . Vector.iterateN (intNat (Proxy :: Proxy n)) f

unsafeIndex :: Vector n a -> Int -> a
unsafeIndex = Vector.unsafeIndex . forgetSize

(!) :: Vector n a -> Int -> a
v ! ix = (Vector.!) (forgetSize v) ix

(!?) :: (KnownNat n) => Vector n a -> Int -> Maybe a
v !? ix = if 0 <= ix && ix < getSize v
          then Just $ unsafeIndex v ix
          else Nothing

tail :: Vector (1+n) a -> Vector n a
tail = Vector . Vector.unsafeTail . forgetSize

head :: Vector (1+n) a -> a
head = Vector.unsafeHead . forgetSize

uncons :: Vector (1+n) a -> (a, Vector n a)
uncons = (head &&& tail)

init :: Vector (n+1) a -> Vector n a
init = Vector . Vector.unsafeInit . forgetSize

last :: Vector (n+1) a -> a
last = Vector.unsafeLast . forgetSize

unsnoc :: Vector (n+1) a -> (Vector n a, a)
unsnoc = (init &&& last)

takeTo :: forall m n. (KnownNat n, KnownNat m,  m <= n) => forall a. Vector n a -> Vector m a
takeTo = let mVal = intNat (Proxy :: Proxy m)
         in Vector . Vector.unsafeTake mVal . forgetSize

dropTo :: forall m n. (KnownNat n, KnownNat m, m <= n) => forall a. Vector n a -> Vector m a
dropTo = let mVal = intNat (Proxy :: Proxy m)
             nVal = intNat (Proxy :: Proxy n)
         in Vector . Vector.unsafeDrop (nVal - mVal) . forgetSize

split :: forall n. (KnownNat n) => forall a m. Vector (n + m) a -> (Vector n a, Vector m a)
split (Vector v) = let nVal = intNat (Proxy :: Proxy n)
                   in  (Vector *** Vector) $ Vector.splitAt nVal v

separate :: forall n m. (KnownNat n, KnownNat m) => forall a. Vector (n * m) a -> Vector n (Vector m a)
separate (Vector v) = let mVal  = intNat (Proxy :: Proxy m)
                          chunk ix = Vector $ Vector.unsafeSlice (ix * mVal) mVal v
                      in generate chunk

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}

module Data.Vector.Fixed
       (Vector
       ,getSize
       ,tail
       ,head
       ,uncons
       ,split
       ,separate
       ,(!)
       ) where

import GHC.Exts (IsList, fromList, toList, Item)
import GHC.TypeLits
import Data.Proxy (Proxy(..))

import qualified Data.Vector as Vector

import Data.Foldable (foldMap, Foldable)
import Data.Traversable (Traversable, sequenceA)
import Control.Applicative (Applicative, (<$>), pure, (<*>))
import Data.Distributive (Distributive, distribute)

import Prelude hiding (tail, head)
import Control.Arrow ((***),(&&&))

newtype Vector (n :: Nat) a = Vector {forgetSize :: Vector.Vector a}
                            deriving(Show)

instance (KnownNat n) => Functor (Vector n) where
  fmap f = Vector . fmap f . forgetSize

instance KnownNat n => Applicative (Vector n) where
  pure = Vector . Vector.replicate (intNat (Proxy :: Proxy n))
  Vector fs <*> Vector xs = Vector $ Vector.zipWith ($) fs xs

instance KnownNat n => Foldable (Vector n) where
  foldMap f = foldMap f . forgetSize

instance KnownNat n => Traversable (Vector n) where
  sequenceA = fmap Vector . sequenceA . forgetSize

instance KnownNat n => Distributive (Vector n) where
  distribute xs = fromFun (\ix -> (!ix) <$> xs)

instance KnownNat n => IsList (Vector n a) where
  type Item (Vector n a) = a
  toList   = Vector.toList . forgetSize
  fromList = let nVal = intNat (Proxy :: Proxy n)
             in Vector . Vector.fromList . (\xs -> let l = length xs
                                                   in if l == nVal
                                                      then xs
                                                      else error $ Prelude.concat
                                                           ["length of list ",
                                                            "(", show l ,")",
                                                            " is less than size of vector ",
                                                            "(", show nVal, ")",
                                                            " in Fixed.fromList"])

intNat :: KnownNat n => Proxy n -> Int
intNat = fromIntegral . natVal

getSize :: KnownNat n => Vector n a -> Int
getSize (_ :: Vector n a)  = intNat (Proxy :: Proxy n)

fromFun :: forall n. KnownNat n => forall a. (Int -> a) -> Vector n a
fromFun = Vector . Vector.generate (intNat (Proxy :: Proxy n))

(!) :: KnownNat n => Vector n a -> Int -> a
v ! ix = (Vector.!) (forgetSize v) ix

tail :: (KnownNat n) => Vector (n+1) a -> Vector n a
tail = Vector . Vector.unsafeTail . forgetSize

head :: (KnownNat n) => Vector (n+1) a -> a
head = Vector.unsafeHead . forgetSize

uncons :: (KnownNat n) => Vector (n+1) a -> (a, Vector n a)
uncons = (head &&& tail)

split :: forall n m. (KnownNat n, KnownNat m) => forall a. Vector (n+m) a -> (Vector n a, Vector m a)
split (Vector v) = let nVal = intNat (Proxy :: Proxy n)
                   in  (Vector *** Vector) $ Vector.splitAt nVal v

separate :: forall n m. (KnownNat n, KnownNat m) => forall a. Vector (n*m) a -> Vector n (Vector m a)
separate (Vector v) = let mVal  = intNat (Proxy :: Proxy m)
                          chunk ix = Vector $ Vector.unsafeSlice (ix * mVal) mVal v
                      in fromFun chunk

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable          #-}

module Data.Vector.Fixed.Size where

import GHC.TypeLits
import Data.Proxy
import Prelude

data Size = N Nat
          | SizeSum Size Size
          | SizeProduct Size Size

type Zero = N 0
type One  = S Zero 

infixl 6 +
infixl 7 *
type (S a)   = SizeSum (N 1) a    
type (a + b) = SizeSum a b
type (a * b) = SizeProduct a b 

class Known (a :: Size) where
  getNat :: Proxy a -> Integer

instance (KnownNat a) => Known (N a) where
  getNat _ = natVal (Proxy :: Proxy a)
  
instance (Known a, Known b) => Known (SizeSum a b) where
  getNat _ = getNat (Proxy :: Proxy a) + getNat (Proxy :: Proxy b)

instance (Known a, Known b) => Known (SizeProduct a b) where
  getNat _ = getNat (Proxy :: Proxy a) * getNat (Proxy :: Proxy b)

getInt :: (Known a) => Proxy a -> Int
getInt = fromIntegral . getNat 

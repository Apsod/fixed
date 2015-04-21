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
          | S Size 
          | Sum Size Size
          | Product Size Size

type (a + b) = Sum a b
type (a * b) = Product a b 

class Known (a :: Size) where
  getNat :: Proxy a -> Integer

instance (KnownNat a) => Known (N a) where
  getNat _ = natVal (Proxy :: Proxy a)

instance (Known a) => Known (S a) where
  getNat _ = (+1) $ getNat (Proxy :: Proxy a) 

instance (Known a, Known b) => Known (Sum a b) where
  getNat _ = getNat (Proxy :: Proxy a) + getNat (Proxy :: Proxy b)

instance (Known a, Known b) => Known (Product a b) where
  getNat _ = getNat (Proxy :: Proxy a) * getNat (Proxy :: Proxy b)

getInt :: (Known a) => Proxy a -> Int
getInt = fromIntegral . getNat 

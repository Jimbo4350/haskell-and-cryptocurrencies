{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module GHC where

import GHC.Generics
import Types

class GEq' f where

    geq' :: f a -> f a -> Bool

class GEq a where

    geq :: a -> a -> Bool
    default geq :: (Generic a, GEq' (Rep a)) => a -> a -> Bool
    geq x y = geq' (from x) (from y)

instance GEq' U1 where

    geq' U1 U1 = True

instance GEq' f => GEq' (M1 i c f) where

    geq' (M1 x) (M1 y) = geq' x y

instance (GEq' f, GEq' g) => GEq' (f :+: g) where

    geq' (L1 x) (L1 x') = geq' x x'
    geq' (R1 y) (R1 y') = geq' y y'
    geq' _      _       = False

instance (GEq' f, GEq' g) => GEq' (f :*: g) where

    geq' (x :*: y) (x' :*: y') = geq' x x' && geq' y y'

instance GEq c => GEq' (K1 i c) where

    geq' (K1 x) (K1 y) = geq x y

deriving instance Generic T
deriving instance Generic Choice
deriving instance Generic (Tree a)
deriving instance Generic (Rose a)

instance GEq Int where
    geq = (==)

instance GEq Char where
    geq = (==)

instance GEq Bool
instance GEq T
instance GEq Choice
instance GEq a => GEq (Tree a)
instance GEq a => GEq [a]
instance GEq a => GEq (Rose a)
instance (GEq a, GEq b) => GEq (a, b)

data Perfect a = Z a | Suc (Perfect (a, a))
    deriving (Generic, GEq)

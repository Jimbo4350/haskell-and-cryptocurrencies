{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module SOP where

import           Generics.SOP
import qualified GHC.Generics as GHC
import           Types

deriving instance GHC.Generic T
deriving instance GHC.Generic Choice
deriving instance GHC.Generic (Tree a)
deriving instance GHC.Generic (Rose a)

deriving instance Generic T
deriving instance Generic Choice
deriving instance Generic (Tree a)
deriving instance Generic (Rose a)

class GEq a where

    geq :: a -> a -> Bool
    default geq :: (Generic a, GEq (SOP I (Code a))) => a -> a -> Bool
    geq x y = geq (from x) (from y)

instance All GEq xs => GEq (NP I xs) where

    geq Nil Nil             = True
    geq (x :* xs) (y :* ys) = geq (unI x) (unI y) && geq xs ys

instance All2 GEq xss => GEq (NS (NP I) xss) where

    geq (Z x) (Z y)                           = geq x y
    geq (Generics.SOP.S x) (Generics.SOP.S y) = geq x y
    geq _                  _                  = False

instance (GEq (NS (NP f) xss)) => GEq (SOP f xss) where
    geq (SOP x) (SOP y) = geq x y

instance GEq Int where
    geq = (==)

instance GEq Char where
    geq = (==)

deriving instance GEq Bool
deriving instance GEq T
deriving instance GEq Choice
deriving instance GEq a => GEq (Tree a)
deriving instance GEq a => GEq [a]
deriving instance GEq a => GEq (Rose a)

{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ser where

import GHC.Generics
import Types

class Ser a where

    ser :: a -> String
    default ser :: (Generic a, GSer (Rep a)) => a -> String
    ser a = gser $ from a

    des :: String -> Maybe (a, String)
    default des :: (Generic a, GSer (Rep a)) => String -> Maybe (a, String)
    des s = do
        (fa, s') <- gdes s
        return $ (to fa, s')

class GSer f where

    gser :: f a -> String
    gdes :: String -> Maybe (f a, String)

instance Ser Char where

    ser c = [c]

    des []       = Nothing
    des (x : xs) = Just (x, xs)

instance Ser Int where

    ser n = show n ++ "*"

    des s = case readsPrec 0 s of
        [(n, ('*' : s'))] -> Just (n, s')
        _                 -> Nothing

instance (GSer f, GSer g) => GSer (f :*: g) where

    gser (fa :*: ga) = gser fa ++ gser ga

    gdes s = do
        (fa, s')  <- gdes s
        (ga, s'') <- gdes s'
        return (fa :*: ga, s'')

instance (GSer f, GSer g) => GSer (f :+: g) where

    gser (L1 fa) = "L" ++ gser fa
    gser (R1 ga) = "R" ++ gser ga

    gdes []         = Nothing
    gdes ('L' : s) = do
        (fa, s') <- gdes s
        return (L1 fa, s')
    gdes ('R' : s) = do
        (ga, s') <- gdes s
        return (R1 ga, s')
    gdes _         = Nothing

instance GSer f => GSer (M1 i c f) where

    gser (M1 fa) = gser fa

    gdes s = do
        (fa, s') <- gdes s
        return (M1 fa, s')

instance Ser c => GSer (K1 i c) where

    gser (K1 c) = ser c

    gdes s = do
        (c, s') <- des s
        return (K1 c, s')

instance GSer U1 where

    gser U1 = ""

    gdes s = Just (U1, s)

deriving instance Generic T
deriving instance Generic Choice
deriving instance Generic (Tree a)
deriving instance Generic (Rose a)

deriving instance Ser a => Ser [a]
deriving instance Ser T
deriving instance Ser Bool
deriving instance Ser Choice
deriving instance Ser a => Ser (Tree a)
deriving instance Ser a => Ser (Rose a)
deriving instance (Ser a, Ser b) => Ser (a, b)

data Perfect a = Zero a | Suc (Perfect (a, a))
    deriving (Show, Generic, Ser)

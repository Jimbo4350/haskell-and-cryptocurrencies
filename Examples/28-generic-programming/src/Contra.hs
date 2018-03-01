{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeOperators     #-}

module Contra where

import GHC.Generics

class Contra f where

    contra :: (a -> b) -> f b -> f a
    default contra :: (Generic1 f, Contra (Rep1 f)) => (a -> b) -> f b -> f a
    contra g x = to1 $ contra g $ from1 x

instance (Contra f, Contra g) => Contra (f :*: g) where

    contra h (x :*: y) = contra h x :*: contra h y

instance (Contra f, Contra g) => Contra (f :+: g) where

    contra h (L1 x) = L1 $ contra h x
    contra h (R1 x) = R1 $ contra h x

instance Contra (K1 i c) where

    contra _ (K1 c) = K1 c

instance Contra f => Contra (M1 i c f) where

    contra h (M1 x) = M1 $ contra h x

instance Contra U1 where

    contra _ U1 = U1

instance Contra f => Contra (Rec1 f) where

    contra h (Rec1 x) = Rec1 (contra h x)

newtype a :<-: b = Into (b -> a)

instance Contra ((:<-:) a) where

    contra f (Into g) = Into (g . f)

data Product f g a = Product (f a) (g a)
    deriving (Generic1, Contra)

data Sum f g a = InL (f a) | InR (g a)
    deriving (Generic1, Contra)

newtype K a b = K a
    deriving (Generic1, Contra)

data P a = P
    deriving (Generic1, Contra)

type Test = Product ((:<-:) String) (Product (K Bool) P)

tString :: Test String
tString = Product (Into $ \s -> s ++ s) $ Product (K True) P

tInt :: Test Int
tInt = contra show tString

test :: (String, Bool)
test =
    let Product (Into f) (Product (K b) P) = tInt
    in  (f 42, b)

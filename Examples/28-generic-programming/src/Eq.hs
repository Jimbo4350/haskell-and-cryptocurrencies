{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Eq where

import Types

class Eq' a where
    eq :: a -> a -> Bool

instance Eq' Int where
    eq = (==)

instance Eq' Char where
    eq = (==)

instance Eq' Bool where

    eq True  True  = True
    eq False False = True
    eq _     _     = False

instance Eq' T where

    eq A         A           = True
    eq (N t1 t2) (N t1' t2') = eq t1 t1' && eq t2 t2'
    eq _         _           = False

instance Eq' Choice where

    eq (I n)   (I n')    = eq n n'
    eq (C c)   (C c')    = eq c c'
    eq (B c b) (B c' b') = eq c c' && eq b b'
    eq (S c)   (S c')    = eq c c'
    eq _       _         = False

instance Eq' a => Eq' (Tree a) where

    eq (Leaf a)   (Leaf a')    = eq a a'
    eq (Node l r) (Node l' r') = eq l l' && eq r r'
    eq _          _            = False

instance Eq' a => Eq' [a] where

    eq [] []             = True
    eq (x : xs) (y : ys) = eq x y && eq xs ys
    eq _        _        = False

instance Eq' a => Eq' (Rose a) where

    eq (Fork a xs) (Fork b ys) = eq a b && eq xs ys

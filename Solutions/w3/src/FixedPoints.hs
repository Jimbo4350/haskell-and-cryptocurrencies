{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : FixedPoints
Description : fixed points
Copyright   : (c) Lars Bruenjes, 2017
License     : BSD3
Maintainer  : lars.bruenjes@iohk.io
Stability   : experimental
Portability : portable

This module contains sample solutions
for the Fixed Points assignment.
-}

module FixedPoints where

import Control.Monad.Fix
import Test.QuickCheck

-- |A version of @'map'@ without explicit recursion,
-- using @'fix'@ instead.
--
-- >>> map' show [True, False]
-- ["True","False"]
map' :: (a -> b) -> [a] -> [b]
map' f = fix $ \m xs -> case xs of
    []        -> []
    (x : xs') -> f x : m xs'

-- |Function @'mystery'@ is a version of @'iterate'@
-- written using @'fix'@.
--
-- >>> take 4 $ mystery (\x -> x * x) (2 :: Int)
-- [2,4,16,256]
mystery :: (a -> a) -> a -> [a]
mystery f x = fix $ (x :) . map f

newtype Fix f = In { out :: f (Fix f) }

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show, Eq)

-- |Generates a random tree with a given number of leaves.
genTree :: Arbitrary a => Int -> Gen (Tree a)
genTree n
    | n <= 1    = Leaf <$> arbitrary
    | otherwise = do
        nl <- elements [1 .. n - 1]
        Node <$> genTree nl <*> genTree (n - nl)

instance Arbitrary a => Arbitrary (Tree a) where

    arbitrary :: Gen (Tree a)
    arbitrary = sized $ \n -> genTree (n + 1)

data TreeF a b = LeafF a | NodeF b b
    deriving Show

instance Show a => Show (Fix (TreeF a)) where

    show (In (LeafF a))   = "(In (LeafF " ++ show a ++ "))"
    show (In (NodeF l r)) = "(In (NodeF " ++ show l ++ " " ++ show r ++ "))"

-- |Converts a tree into a fixpoint of @'TreeF'@.
fromTree :: Tree a -> Fix (TreeF a)
fromTree (Leaf a)   = In $ LeafF a
fromTree (Node l r) = In $ NodeF (fromTree l) (fromTree r)

-- |Converts a fixpoint of @'TreeF'@ into a tree.
toTree :: Fix (TreeF a) -> Tree a
toTree (In (LeafF a))   = Leaf a
toTree (In (NodeF l r)) = Node (toTree l) (toTree r)

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Wno-unused-imports #-}
module HigherOrder where

import Data.List (foldl', sortBy)
import Prelude hiding (take, product, reverse, all)

-- These are binary trees with labels in their nodes.

data BinTree a =
    Bin (BinTree a) a (BinTree a)
  | Empty
  deriving (Eq, Show)

-- Task HigherOrder-1.
--
-- Define 'product' both using an accumulator explicitly,
-- and using (strict) foldl'.

product :: Num a => [a] -> a
product = go 1
  where
    go :: Num a => a -> [a] -> a
    go !acc []       = acc
    go !acc (x : xs) = go (acc * x) xs

product' :: Num a => [a] -> a
product' = foldl' (*) 1

-- Task HigherOrder-2.
--
-- Define 'reverse' using 'foldl'.

reverse :: [a] -> [a]
reverse = foldl' (flip (:)) []

-- Task HigherOrder-3.
--
-- Define a Functor instance for binary trees. For
-- this, we have to define a map function on binary
-- trees and then define the class instance.
--
-- The instance is actually given below. You just
-- have to uncomment it.

mapBinTree :: (a -> b) -> BinTree a -> BinTree b
mapBinTree f (Bin l x r) = Bin (mapBinTree f l) (f x) (mapBinTree f r)
mapBinTree _ Empty       = Empty

instance Functor BinTree where
  fmap = mapBinTree

-- Task HigherOrder-4.
--
-- The 'BinTree' type is suitable for representing
-- "binary search trees".
--
-- Binary search trees are trees that store their elements
-- in order, so that we can efficiently find elements by comparing
-- the element we are looking for with the current node, and
-- descending either left or right.
--
-- Define a function 'isBST' that checks if a given 'BinTree'
-- is a binary search tree.

isBST :: Ord a => BinTree a -> Bool
isBST t = case isBST' t of
    Left () -> False
    Right _ -> True
  where
    isBST' :: Ord a => BinTree a -> Either () (Maybe (a, a))
    isBST' (Bin l x r) = case (isBST' l, isBST' r) of
        (Left ()                  , _)                         -> Left ()
        (_                        , Left ())                   -> Left ()
        (Right Nothing            , Right Nothing)             -> Right (Just (x, x))
        (Right Nothing            , Right (Just (rMin, rMax)))
            | x >= rMin                                        -> Left ()
            | otherwise                                        -> Right (Just (x, rMax))
        (Right (Just (lMin, lMax)), Right Nothing)
            | x <= lMax                                        -> Left ()
            | otherwise                                        -> Right (Just (lMin, x))
        (Right (Just (lMin, lMax)), Right (Just (rMin, rMax)))
            | x <= lMax || x >= rMin                           -> Left ()
            | otherwise                                        -> Right (Just (lMin, rMax))
    isBST' Empty       = Right Nothing

-- Task HigherOrder-5.
--
-- Define a function 'search' that looks up a value in a BST.
--
-- From now on, we use a type synonym to signal that a certain
-- binary tree should in fact be a binary search tree, even if
-- the type system does not actively enforce this.

type BST a = BinTree a

search :: Ord a => a -> BST a -> Bool
search _ Empty = False
search a (Bin l x r)
    | a < x     = search a l
    | a > x     = search a r
    | otherwise = True

-- Task HigherOrder-6.
--
-- Define a function 'insert' that inserts a value into a BST
-- while maintaining the BST property. (Don't worry about balancing
-- the tree. That's not important for now. But do make sure you
-- maintain the BST property itself.)

insert :: Ord a => a -> BST a -> BST a
insert a Empty         = Bin Empty a Empty
insert a t@(Bin l x r)
    | a < x            = Bin (insert a l) x r
    | a > x            = Bin l x (insert a r)
    | otherwise        = t

-- Task HigherOrder-7.
--
-- Define the function 'all' (as in the Prelude) using 'foldr'.
-- Hide the original binding from the Prelude by exluding it in
-- the module header. Provide the type signature yourself.

-- TODO: define all

all :: Foldable t => (a -> Bool) -> t a -> Bool
all f = foldl' (\b a -> b && f a) True

-- Task HigherOrder-8.
--
-- Import the function 'sortyBy' from the 'Data.List' module.
-- Then use this function to define a function that sorts a
-- list in descending rather than ascending order.

sortDescending :: Ord a => [a] -> [a]
sortDescending = sortBy (flip compare)

-- Task HigherOrder-9.
--
-- Use 'insert' and 'foldr' to create a BST from a list.

fromListBST :: Ord a => [a] -> BST a
fromListBST = foldr insert Empty

-- Task HigherOrder-10.
--
-- We want to attach unique numbers to each node in a binary
-- tree, so that all the numbers from left to right are labelled
-- in ascending order.
--
-- NOTE: This is not easy. Think about this and discuss your
-- strategy with us before you proceed.

labelTree :: BinTree a -> BinTree (a, Int)
labelTree t = fst $ go 1 t
  where
    go :: Int -> BinTree a -> (BinTree (a, Int), Int)
    go n Empty       = (Empty, n)
    go n (Bin l x r) =
        let (l', n1) = go n l
            x'       = (x, n1)
            (r', n2) = go (n1 + 1) r
        in  (Bin l' x' r', n2)

-- Task HigherOrder-11.
--
-- Another form of tree labeling does not use an integer, but
-- a label supply that is given as a list. So write a variant
-- of 'labelTree' that takes the labels from a list, but uses
-- every label only once. You may assume in this function that
-- the list contains infinitely many (or at least sufficiently
-- many) labels, so you don't have to return a 'Maybe' if the
-- list is too short, but can just crash.

labelTree' :: BinTree a -> [b] -> BinTree (a, b)
labelTree' t ls = fst $ go ls t
  where
    go :: [b] -> BinTree a -> (BinTree (a, b), [b])
    go ys Empty       = (Empty, ys)
    go ys (Bin l x r) =
        let (l', y : ys1) = go ys l
            x'             = (x, y)
            (r', ys2)      = go ys1 r
        in  (Bin l' x' r', ys2)

-- Task HigherOrder-12.
--
-- Define the catamorphism on 'BinTree'.
-- Also come up with the type signature yourself.
-- Look at functions such as 'mapBinTree' and 'search'
-- above for inspiration. Also try to
-- rewrite these in terms of the catamorphism once you
-- are done.

binTree :: b -> (b -> a -> b -> b) -> BinTree a -> b
binTree empty _   Empty       = empty
binTree empty bin (Bin l x r) = bin (binTree empty bin l) x (binTree empty bin r)

mapBinTree' :: (a -> b) -> BinTree a -> BinTree b
mapBinTree' f = binTree Empty (\l x r -> Bin l (f x) r)

search' :: Ord a => a -> BST a -> Bool
search' a = binTree False f
  where
    f l x r
        | a < x     = l
        | a > x     = r
        | otherwise = True

-- Task HigherOrder-13.
--
-- Try to implement the function 'take' on lists using 'foldr'.
--
-- Once again, this is not easy, and you should discuss your
-- ideas with us before trying.
--
-- Consider the type signature and a possible definition of
-- take, and note that not just the list is being traversed,
-- but also the number changes.

take :: Int -> [a] -> [a]
take _ []       = []
take n (x : xs)
  | n > 0     = x : take (n - 1) xs
  | otherwise = []

take' :: Int -> [a] -> [a]
take' = flip take''
  where
    take'' :: [a] -> Int -> [a]
    take'' = foldr f (const [])

    f :: a -> (Int -> [a]) -> (Int -> [a])
    f a g n
        | n > 0     = a : g (n - 1)
        | otherwise = []

-- Task HigherOrder-14.
--
-- If you succeeded in defining 'take' in terms of 'foldr',
-- then perhaps it will not surprise you all that much that
-- even 'foldl' can be written in terms of 'foldr'.
--
-- Try to do this. The approach required is similar.

foldl'' :: forall t a b. Foldable t => (b -> a -> b) -> b -> t a -> b
foldl'' f = flip foldl'''
  where
    foldl''' :: Foldable t => t a -> b -> b
    foldl''' = foldr h id

    h :: a -> (b -> b) -> (b -> b)
    h a i b = i (f b a)

-- Task HigherOrder-15.
--
-- Define a Foldable instance for binary trees.
-- For this, there are several methods, but the
-- easiest with our knowledge so far is to implement
-- a foldr function on trees.
-- For this, one option might be to first convert a
-- binary tree to a list.

foldrBinTree :: (a -> b -> b) -> b -> BinTree a -> b
foldrBinTree f b = foldr f b . toListBinTree
  where
    toListBinTree :: BinTree a -> [a]
    toListBinTree Empty       = []
    toListBinTree (Bin l x r) = toListBinTree l ++ [x] ++ toListBinTree r

instance Foldable BinTree where
  foldr = foldrBinTree

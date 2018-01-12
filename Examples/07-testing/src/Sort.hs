module Sort where

--------------------------------------------------------------------------
-- Introduction:
--
-- This module accompanies the part on testing.
--
-- Only the original version of the code, and some of the properties are
-- provided. You can perform the changes yourself.

--------------------------------------------------------------------------
-- Insertion sort code:

sort :: [Int] -> [Int] -- not correct
sort []      =  []
sort (x:xs)  =  insert x (sort xs)

insert :: Int -> [Int] -> [Int] -- not correct
insert x []                     =  [x]
insert x (y:ys)  | x <= y       =  x : ys
                 | otherwise    =  y : insert x ys

evilSort :: Ord a => [a] -> [a]
evilSort []         = []
evilSort xs@(x : _) = replicate (length xs) x

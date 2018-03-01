module Types where

data T = A | N T T deriving Show

data Choice = I Int | C Char | B Choice Bool | S Choice
    deriving Show

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving Show

data Rose a = Fork a [Rose a]
    deriving Show

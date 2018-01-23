{-# LANGUAGE ScopedTypeVariables #-}

module TailRecursion
  ( Tree (..)
  , splitLeft
  , splitLeft'
  , splitLeftCont
  ) where

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

splitLeft :: Tree a -> (a, Maybe (Tree a))
splitLeft (Leaf a)   = (a, Nothing)
splitLeft (Node l r) = case splitLeft l of
  (a, Nothing) -> (a, Just r)
  (a, Just l') -> (a, Just (Node l' r))

-- |@`splitLeft'`@ just calls the helper function
-- @`splitLeftCont`@ with @`id`@ as continuation.
splitLeft' :: Tree a -> (a, Maybe (Tree a))
splitLeft' t = splitLeftCont t id

splitLeftCont :: forall a b.
                 Tree a                -- ^the tree
              -> (Maybe (Tree a) -> b) -- ^the continuation
              -> (a, b)                -- ^returns the left-most element and the result 
                                       -- of applying the continuation to the (optional) rest of the tree.
splitLeftCont (Leaf a)   cont = (a, cont Nothing)
splitLeftCont (Node l r) cont =
  let f :: Maybe (Tree a) -> Tree a
      f Nothing   = r
      f (Just l') = Node l' r
  in  splitLeftCont l (cont . Just . f)

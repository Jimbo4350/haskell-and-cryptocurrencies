module Phantom where

data Zero

data Suc n

newtype Vec n a = Vec [a] deriving Show

nil :: Vec Zero a
nil = Vec []

cons :: a -> Vec n a -> Vec (Suc n) a
cons x (Vec xs) = Vec (x : xs)


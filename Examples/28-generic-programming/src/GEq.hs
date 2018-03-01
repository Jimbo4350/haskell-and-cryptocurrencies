{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module GEq where

import Types

data U = U deriving Show
data a :+: b = L a | R b deriving Show
data a :*: b = a :*: b deriving Show

class Generic a where

    type Rep a
    from :: a -> Rep a
    to :: Rep a -> a

instance Generic Bool where

    type Rep Bool = U :+: U

    from False = L U
    from True  = R U

    to (L U) = False
    to (R U) = True

instance Generic T where

    type Rep T = U :+: (T :*: T)

    from A       = L U
    from (N l r) = R (l :*: r)

    to (L U)         = A
    to (R (l :*: r)) = N l r

instance Generic Choice where

    type Rep Choice = Int :+: (Char :+: ((Choice :*: Bool) :+: Choice))

    from (I n)   = L n
    from (C c)   = R (L c)
    from (B c b) = R (R (L (c :*: b)))
    from (S c)   = R (R (R c))

    to (L n)                 = I n
    to (R (L c))             = C c
    to (R (R (L (c :*: b)))) = B c b
    to (R (R (R c)))         = S c

instance Generic (Tree a) where

    type Rep (Tree a) = a :+: (Tree a :*: Tree a)

    from (Leaf a)   = L a
    from (Node l r) = R (l :*: r)

    to (L a)         = Leaf a
    to (R (l :*: r)) = Node l r

instance Generic [a] where

    type Rep [a] = U :+: (a :*: [a])

    from []       = L U
    from (x : xs) = R (x :*: xs)

    to (L U)          = []
    to (R (x :*: xs)) = x : xs

instance Generic (Rose a) where

    type Rep (Rose a) = a :*: [Rose a]

    from (Fork a xs) = a :*: xs

    to (a :*: xs) = Fork a xs

class GEq a where

    geq :: a -> a -> Bool
    default geq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
    geq x y = geq (from x) (from y)

instance GEq U where

    geq U U = True

instance (GEq a, GEq b) => GEq (a :+: b) where

    geq (L a) (L a') = geq a a'
    geq (R b) (R b') = geq b b'
    geq _     _      = False

instance (GEq a, GEq b) => GEq (a :*: b) where

    geq (a :*: b) (a' :*: b') = geq a a' && geq b b'

instance GEq Int where
    geq = (==)

instance GEq Char where
    geq = (==)

instance GEq Bool
instance GEq T
instance GEq Choice
instance GEq a => GEq (Tree a)
instance GEq a => GEq [a]
instance GEq a => GEq (Rose a)

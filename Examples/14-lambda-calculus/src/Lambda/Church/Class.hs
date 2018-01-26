module Lambda.Church.Class
    ( Church (..)
    ) where

import Lambda.Church.Bool
import Lambda.Church.Either
import Lambda.Church.List
import Lambda.Church.Maybe
import Lambda.Church.Natural
import Lambda.Church.Pair
import Lambda.Combinators
import Lambda.Core
import Lambda.Reduction

class Church a where
    toTerm :: a -> Term
    fromTerm :: Term -> Maybe a

instance Church Bool where

    toTerm True  = true
    toTerm False = false

    fromTerm t =
        let t' = t :$ "true" :$ "false"
        in  case run cbn t' of
                (Var "true")  -> Just True
                (Var "false") -> Just False
                _             -> Nothing

instance Church Natural where

    toTerm 0 = zero
    toTerm n = run cbn $ succ' :$ toTerm (n - 1)

    fromTerm t =
        let t'  = run cbn $ t :$ "f" :$ "x"
        in  go t'
      where
        go (Var "x")      = Just 0
        go (Var "f" :$ u) = (1+) <$> go u
        go _              = Nothing

instance (Church a, Church b) => Church (a, b) where

    toTerm (a, b) = pair :$ toTerm a :$ toTerm b

    fromTerm t = (,) <$> fromTerm (fst' :$ t) <*> fromTerm (snd' :$ t)

instance Church a => Church [a] where

    toTerm []       = nil
    toTerm (x : xs) = normalize $ cons :$ toTerm x :$ toTerm xs

    fromTerm t = do
        b <- fromTerm $ isNil :$ t
        if b then return []
             else do
                 x  <- fromTerm $ head' :$ t
                 xs <- fromTerm $ tail' :$ t
                 return (x : xs)

instance (Church a, Church b) => Church (Either a b) where

    toTerm = normalize . either ((left' :$) . toTerm) ((right' :$) . toTerm)

    fromTerm t = do
        b <-fromTerm $ isLeft' :$ t
        let u = either' :$ t :$ i :$ i
        if b then Left  <$> fromTerm u
             else Right <$> fromTerm u

instance Church a => Church (Maybe a) where

    toTerm = normalize . maybe nothing ((just :$) . toTerm)

    fromTerm t = do
        b <- fromTerm $ isNothing' :$ t
        if b then return Nothing
             else Just <$> fromTerm (maybe' :$ t :$ false :$ i)

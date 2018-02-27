{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module GADT where

import Data.Text (Text)
import Prelude   hiding (foldr, head, minimum, tail)

--data Maybe a = Nothing | Just a

--data Maybe :: * -> * where
--    Nothing :: Maybe a
--    Just    :: a -> Maybe a

infixr 5 :*

data Vec :: Nat -> * -> * where
    Nil  :: Vec 'Zero a
    (:*) :: a -> Vec n a -> Vec ('Suc n) a

deriving instance Show a => Show (Vec n a)

data Nat = Zero | Suc Nat deriving Show

newtype Question = Q Text deriving Show

newtype Answer = A Bool deriving Show

type Two = 'Suc ('Suc 'Zero)

exampleQ :: Vec Two Question
exampleQ =    Q "Do you like Haskell?"
           :* Q "Do you like dynamic types?"
           :* Nil

exampleA :: Vec Two Answer
exampleA =    A True
           :* A False
           :* Nil

type Score = Int

type Scoring = Answer -> Score

yesno :: Score -> Score -> Scoring
yesno yes no (A b) = if b then yes else no

exampleS :: Vec Two Scoring
exampleS =    yesno 5 0
           :* yesno 0 2
           :* Nil

score :: Vec n Scoring -> Vec n Answer -> Score
score ss as = sum $ toList (zipWith' ($) ss as)

toList :: Vec n a -> [a]
toList Nil       = []
toList (x :* xs) = x : toList xs

zipWith' :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
zipWith' _ Nil       Nil       = Nil
zipWith' f (a :* as) (b :* bs) = f a b :* zipWith' f as bs

zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' _ []       _        = []
zipWith'' _ _        []       = []
zipWith'' f (a : as) (b : bs) = f a b : zipWith'' f as bs

deriving instance Functor (Vec n)

head :: Vec ('Suc n) a -> a
head (x :* _) = x

tail :: Vec ('Suc n) a -> Vec n a
tail (_ :* xs) = xs

minimum :: Ord a => Vec ('Suc n) a -> a
minimum (x :* xs) = minimum' x xs

minimum' :: Ord a => a -> Vec n a -> a
minimum' a Nil       = a
minimum' a (x :* xs) = minimum' (min a x) xs

foldr :: (a -> r -> r) -> r -> Vec n a -> r
foldr _  e Nil       = e
foldr op e (x :* xs) = op x (foldr op e xs)

data QType = QYesNo | QQuant deriving Show

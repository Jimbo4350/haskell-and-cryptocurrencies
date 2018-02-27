{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

module GADT2 where

import Data.Text (Text)

infixr 5 :*

data QType :: * -> * where
    QYesNo :: QType Bool
    QQuant :: QType Int

deriving instance Show (QType a)

data Question a = Q Text (QType a)

deriving instance Show (Question a)

data Answer :: * -> * where
    AYesNo :: Bool -> Answer Bool
    AQuant :: Int  -> Answer Int

deriving instance Show (Answer a)

q1 :: Question Int
q1 = Q "How many type errors?" QQuant

q2 :: Question Bool
q2 = Q "Do you like Haskell" QYesNo

a1 :: Answer Int
a1 = AQuant 0

a2 :: Answer Bool
a2 = AYesNo True

data HList :: [*] -> * where
    HNil  :: HList '[]
    HCons :: a -> HList xs -> HList (a ': xs)

infixr 2 `HCons`

data Env :: [*] -> (* -> *) -> * where
  Nil  :: Env '[] f
  (:*) :: f a -> Env as f -> Env (a ': as) f

deriving instance Show (Env xs Question)
deriving instance Show (Env xs Answer)

exampleQ :: Env '[Int, Bool] Question
exampleQ = q1 :* q2 :* Nil

exampleA :: Env '[Int, Bool] Answer
exampleA = a1 :* a2 :* Nil

type Score = Int

newtype Scoring a = S (Answer a -> Score)

yesno :: Score -> Score -> Scoring Bool
yesno yes no = S $ \(AYesNo b) -> if b then yes else no

quantity :: (Int -> Int) -> Scoring Int
quantity f = S $ \(AQuant n) -> f n

exampleS :: Env '[Int, Bool] Scoring
exampleS =    quantity negate
           :* yesno 2 0
           :* Nil

score :: Env xs Scoring -> Env xs Answer -> Int
score Nil Nil = 0
score (S s :* ss) (a :* as) = s a + score ss as


{-
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
-}

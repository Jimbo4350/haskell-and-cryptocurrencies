{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}

module GADT where

import Data.Text (Text)
import Prelude   hiding (zipWith, (!!))

data Nat = Zero | Suc Nat deriving Show

data SNat :: Nat -> * where
    SZero :: SNat 'Zero
    SSuc  :: SNat n -> SNat ('Suc n)

deriving instance Show (SNat n)

data Vec :: Nat -> * -> * where
    VNil  :: Vec 'Zero a
    VCons :: a -> Vec n a -> Vec ('Suc n) a

infixr 5 :*

data QType = QYesNo | QQuant | QArith

data SQType :: QType -> * where
    SQYesNo :: SQType 'QYesNo
    SQQuant :: SQType 'QQuant
    SQArith :: SQType 'QArith

deriving instance Show QType

data Question (a :: QType) = Q Text (SQType a)

{-
deriving instance Show (Question a)
-}

data Answer :: QType -> * where
    AYesNo :: Bool -> Answer 'QYesNo
    AQuant :: Int  -> Answer 'QQuant
    AArith :: Int  -> Answer 'QArith

deriving instance Show (Answer a)

q1 :: Question 'QQuant
q1 = Q "How many type errors?" SQQuant

q2 :: Question 'QYesNo
q2 = Q "Do you like Haskell" SQYesNo

a1 :: Answer 'QQuant
a1 = AQuant 0

a2 :: Answer 'QYesNo
a2 = AYesNo True

data HList :: [*] -> * where
    HNil  :: HList '[]
    HCons :: a -> HList xs -> HList (a ': xs)

infixr 2 `HCons`

data Env :: [k] -> (k -> *) -> * where
  Nil  :: Env '[] f
  (:*) :: f a -> Env as f -> Env (a ': as) f

{-
deriving instance Show (Env xs Question)
deriving instance Show (Env xs Answer)
-}

exampleQ :: Env ['QQuant, 'QYesNo] Question
exampleQ = q1 :* q2 :* Nil

exampleA :: Env ['QQuant, 'QYesNo] Answer
exampleA = a1 :* a2 :* Nil

type Score = Int

newtype Scoring a = S (Answer a -> Score)

yesno :: Score -> Score -> Scoring 'QYesNo
yesno yes no = S $ \(AYesNo b) -> if b then yes else no

quantity :: (Int -> Int) -> Scoring 'QQuant
quantity f = S $ \(AQuant n) -> f n

exampleS :: Env ['QQuant, 'QYesNo] Scoring
exampleS =    quantity negate
           :* yesno 2 0
           :* Nil

score :: Env xs Scoring -> Env xs Answer -> Int
score Nil Nil = 0
score (S s :* ss) (a :* as) = s a + score ss as

score' :: Env xs Scoring -> Env xs Answer -> Int
score' ss as = sum $ toList $ zipWith op ss as
  where
    op :: Scoring a -> Answer a -> K Score a
    op (S s) a = K (s a)

newtype K a b = K {unK :: a} deriving Show

newtype I a = I {unI :: a} deriving Show

toList :: Env xs (K a) -> [a]
toList Nil         = []
toList (K a :* as) = a : toList as

zipWith :: (forall x. f x -> g x -> h x) 
        -> Env xs f -> Env xs g -> Env xs h
zipWith _  Nil       Nil       = Nil
zipWith op (x :* xs) (y :* ys) = op x y :* zipWith op xs ys

data Ptr :: [k] -> k -> * where
    PZero :: Ptr (x ': xs) x
    PSuc  :: Ptr xs y -> Ptr (x ': xs) y

pTwo :: Ptr (x ': y ': z ': xs) z
pTwo = PSuc (PSuc PZero)

(!!) :: Env as f -> Ptr as a -> f a
(!!) (x :* _)  PZero    = x
(!!) (_ :* xs) (PSuc i) = xs !! i

data SomePtr :: [k] -> * where
    SomePtr :: Ptr as a -> SomePtr as

findIndex :: (forall a. f a -> Bool)
          -> Env as f -> Maybe (SomePtr as)
findIndex _ Nil       = Nothing
findIndex p (x :* xs) 
    | p x             = Just (SomePtr PZero)
    | otherwise       = case findIndex p xs of
            Nothing          -> Nothing
            Just (SomePtr i) -> Just (SomePtr (PSuc i))
            
task :: Env as Question -> Env as Answer
     -> (Text -> Bool) -> Maybe String
task qs as p = do
    SomePtr i <- findIndex (\(Q txt _) -> p txt) qs
    let a = as !! i
    return (show a)

data SomeVec :: * -> * where
    SomeVec :: Vec n a -> SomeVec a

deriving instance Show a => Show (Vec n a)

deriving instance Show a => Show (SomeVec a)

replicateV :: Int -> a -> SomeVec a
replicateV 0 _ = SomeVec VNil
replicateV n a = case replicateV (n - 1) a of
    SomeVec v -> SomeVec (a `VCons` v)

fromList :: [a] -> SomeVec a
fromList []       = SomeVec VNil
fromList (x : xs) = case fromList xs of
    SomeVec v -> SomeVec (x `VCons` v)

replicateV' :: SNat n -> a -> Vec n a
replicateV' SZero    _ = VNil
replicateV' (SSuc n) x = x `VCons` replicateV' n x

class SNatI (n :: Nat) where
    snat :: SNat n

instance SNatI 'Zero where
    snat = SZero

instance SNatI n => SNatI ('Suc n) where
    snat = SSuc snat

replicateV'' :: SNatI n => a -> Vec n a
replicateV'' a = replicateV' snat a

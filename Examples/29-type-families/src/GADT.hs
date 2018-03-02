{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module GADT where

import           Prelude hiding ((+))

data Nat = Zero | Suc Nat deriving Show

infixl 6 +

(+) :: Nat -> Nat -> Nat
Zero  + n = n
Suc m + n = Suc (m + n)

data PlusR :: Nat -> Nat -> Nat -> * where
    PlusZ :: PlusR 'Zero n n
    PlusS :: PlusR m n n' -> PlusR ('Suc m) n ('Suc n')

data SNat :: Nat -> * where
    SZero :: SNat 'Zero
    SSuc  :: SNat n -> SNat ('Suc n)

deriving instance Show (SNat n)

data Vec :: Nat -> * -> * where
    VNil  :: Vec 'Zero a
    VCons :: a -> Vec n a -> Vec ('Suc n) a

infixr 5 :*

data Env :: [k] -> (k -> *) -> * where
  Nil  :: Env '[] f
  (:*) :: f a -> Env as f -> Env (a ': as) f

type family (+) (m :: Nat) (n :: Nat) :: Nat where
    (+) 'Zero n = n
    (+) ('Suc m) n = 'Suc (m + n)

append :: Vec m a -> Vec n a -> Vec (m + n) a
append VNil v         = v
append (VCons a as) v = VCons a (append as v)

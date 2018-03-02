{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans      #-}

module Interleave where

import           Data.Constraint
import           Data.Type.Equality
import           GADT

type family (*) (m :: Nat) (n :: Nat) :: Nat where
    'Zero * n = 'Zero
    ('Suc m) * n = n + (m * n)

type Two = 'Suc ('Suc 'Zero)

deriving instance Show (Vec n Char)

interleave :: Vec n a -> Vec n a -> Vec (n + n) a
interleave VNil VNil = VNil
interleave (VCons x xs) (VCons y ys) =
    gcastWith (thmPlusSuc (vlength xs) (vlength ys)) $
    VCons x $ VCons y $ interleave xs ys

thmPlusZero :: SNat n -> (n + 'Zero) :~: n
thmPlusZero SZero     = Refl
thmPlusZero (SSuc n') = gcastWith (thmPlusZero n') Refl

thmPlusSuc :: SNat m -> SNat n -> (m + 'Suc n) :~: 'Suc (m + n)
thmPlusSuc SZero _     = Refl
thmPlusSuc (SSuc m') n = gcastWith (thmPlusSuc m' n) Refl

vlength :: Vec n a -> SNat n
vlength VNil         = SZero
vlength (VCons _ xs) = SSuc $ vlength xs

vreverse :: Vec n a -> Vec n a
vreverse v = gcastWith (thmPlusZero $ vlength v) $ go v VNil
  where
    go :: Vec m a -> Vec n a -> Vec (m + n) a
    go VNil acc         = acc
    go (VCons x xs) acc =
        gcastWith (thmPlusSuc (vlength xs) (vlength acc)) $
        go xs (VCons x acc)

type MyConstraint a = (Eq a, Show a, Read a)

foo :: MyConstraint a => String -> a -> String
foo s a = if read s == a then show a else "FOO"

type family All (c :: k -> Constraint) (xs :: [k]) :: Constraint where
    All c '[]       = ()
    All c (x ': xs) = (c x, All c xs)

type family Map (f :: k -> l) (xs :: [k]) :: [l] where
    Map f '[] = '[]
    Map f (x ': xs) = f x ': Map f xs

deriving instance All Show (Map f xs) => Show (Env xs f)

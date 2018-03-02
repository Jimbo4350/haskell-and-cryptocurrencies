{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

{-|
Module      : Dict
Description : explicit dictionaries
Copyright   : (c) Lars Bruenjes, 2017
License     : BSD3
Maintainer  : lars.bruenjes@iohk.io
Stability   : experimental
Portability : portable

Sample solution for W6.3.
-}

module Dict where

newtype FunctorDict f = FunctorDict {fmap_ :: forall a b. (a -> b) -> f a -> f b}

data Sum f g a = Inl (f a) | Inr (g a)
    deriving Show

instance (Functor f, Functor g) => Functor (Sum f g) where
    fmap h (Inl fa) = Inl (fmap h fa)
    fmap h (Inr ga) = Inr (fmap h ga)

newtype Compose f g a = Compose {getCompose :: f (g a)}
    deriving Show

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap h = Compose . fmap (fmap h) . getCompose

sumFunctor :: FunctorDict f -> FunctorDict g -> FunctorDict (Sum f g)
sumFunctor dictF dictG = FunctorDict $ \h sa -> case sa of
    Inl fa -> Inl (fmap_ dictF h fa)
    Inr ga -> Inr (fmap_ dictG h ga)

composeFunctor :: FunctorDict f -> FunctorDict g -> FunctorDict (Compose f g)
composeFunctor dictF dictG = FunctorDict $ \h ->
    Compose . fmap_ dictF (fmap_ dictG h) . getCompose

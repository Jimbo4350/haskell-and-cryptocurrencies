{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE InstanceSigs #-}

{-|
Module      : Free
Description : free monads
Copyright   : (c) Lars Bruenjes, 2017
License     : BSD3
Maintainer  : lars.bruenjes@iohk.io
Stability   : experimental
Portability : portable

This module contains the free monad construction
from the lecture.
-}

module Free where

import Control.Monad (liftM, ap)

-- |The free monad for the functor @f@.
data Free f a =
      Return a
    | Wrap (f (Free f a))

instance Functor f => Functor (Free f) where
    fmap = liftM

instance Functor f => Applicative (Free f) where
    pure = return
    (<*>) = ap

instance Functor f => Monad (Free f) where
    return :: a -> Free f a
    return = Return

    (>>=) :: Free f a -> (a -> Free f b) -> Free f b
    Return a >>= cont = cont a
    Wrap m   >>= cont = Wrap $ fmap (>>= cont) m

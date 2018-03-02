{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

{-|
Module      : Par
Description : monad-par
Copyright   : (c) Lars Bruenjes, 2017
License     : BSD3
Maintainer  : lars.bruenjes@iohk.io
Stability   : experimental
Portability : portable

Sample solution for W6.5.
-}

module Par where

import Data.IORef (IORef)
import Free

data IVarContents a = Full a | Empty | Blocked [a -> Trace]

newtype IVar a = IVar (IORef (IVarContents a))

data Trace = forall a . Get (IVar a) (a -> Trace)
           | forall a . Put (IVar a) a Trace
           | forall a . New (IVarContents a) (IVar a -> Trace)
           | Fork Trace Trace
           | Done
           | Yield Trace
           | forall a . LiftIO (IO a) (a -> Trace)

newtype Par a = Par {
    runCont :: (a -> Trace) -> Trace
}

-- |We see that @`Trace`@ cannot written using @`Free`@ directly,
-- but we can define the following functor:
data TraceF :: * -> * where
    GetF    :: IVar a -> (a -> b) -> TraceF b
    PutF    :: IVar a -> a -> b -> TraceF b
    NewF    :: IVarContents a -> (IVar a -> b) -> TraceF b
    ForkF   :: b -> b -> TraceF b
    YieldF  :: b -> TraceF b
    LiftIOF :: IO a -> (a -> b) -> TraceF b

deriving instance Functor TraceF

-- |Now we can express @`Trace`@ in terms of @`TraceF`@,
-- using @`Free`@.
type Trace' = Free TraceF ()

-- |Isomorphism witnessing that @`Trace`@
-- is isomorphic to @`Trace'`@.
toTrace :: Trace' -> Trace
toTrace (Return _)           = Done
toTrace (Wrap (GetF v k))    = Get v (toTrace . k)
toTrace (Wrap (PutF v a t))  = Put v a (toTrace t)
toTrace (Wrap (NewF c k))    = New c (toTrace . k)
toTrace (Wrap (ForkF t t'))  = Fork (toTrace t) (toTrace t')
toTrace (Wrap (YieldF t))    = Yield (toTrace t)
toTrace (Wrap (LiftIOF m k)) = LiftIO m (toTrace . k)

-- |Isomorphism in the other direction witnessing that @`Trace`@
-- is isomorphic to @`Trace'`@.
fromTrace :: Trace -> Trace'
fromTrace Done         = Return ()
fromTrace (Get v k)    = Wrap $ GetF v $ fromTrace . k
fromTrace (Put v a t)  = Wrap $ PutF v a $ fromTrace t
fromTrace (New c k)    = Wrap $ NewF c $ fromTrace . k
fromTrace (Fork t t')  = Wrap $ ForkF (fromTrace t) (fromTrace t')
fromTrace (Yield t)    = Wrap $ YieldF $ fromTrace t
fromTrace (LiftIO m k) = Wrap $ LiftIOF m $ fromTrace . k

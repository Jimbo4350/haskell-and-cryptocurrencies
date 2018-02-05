{-# LANGUAGE InstanceSigs #-}

module Reader
    ( Reader (..)
    , ask
    , local
    , Env
    , EnvError (..)
    ) where

import           Control.Monad
import           Data.Map      (Map)

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap = liftM

instance Applicative (Reader r) where
    pure = return
    (<*>) = ap

instance Monad (Reader r) where

    return :: a -> Reader r a
    return a = Reader $ const a

    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    ra >>= cont = Reader $ \r -> runReader (cont $ runReader ra r) r

ask :: Reader r r
ask = Reader id

local :: (r -> r) -> Reader r a -> Reader r a
local f ra = Reader $ runReader ra .  f

type Env = Map String Int

newtype EnvError a = EnvError { runEnvError :: Env -> Either String a }

instance Functor EnvError where
    fmap = liftM

instance Applicative EnvError where
    pure = return
    (<*>) = ap

instance Monad EnvError where

    return :: a -> EnvError a
    return a = EnvError $ \_ -> Right a

    (>>=) :: EnvError a -> (a -> EnvError b) -> EnvError b
    ea >>= cont = EnvError $ \env -> case runEnvError ea env of
        Left e  -> Left e
        Right a -> runEnvError (cont a) env


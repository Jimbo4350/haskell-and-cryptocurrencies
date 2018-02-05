{-# LANGUAGE InstanceSigs #-}

module EnvError
    ( Env
    , EnvError (..)
    , ask
    , local
    ) where

import           Control.Monad
import           Data.Map      (Map)

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

ask :: EnvError Env
ask = EnvError Right

local :: (Env -> Env) -> EnvError a -> EnvError a
local f ea = EnvError $ runEnvError ea . f

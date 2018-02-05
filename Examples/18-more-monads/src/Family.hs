{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Family
    ( MonadReader (..)
    , withDouble
    ) where

import           EnvError (Env, EnvError (..))
import qualified EnvError as E
import           Reader   (Reader (..))
import qualified Reader   as R

type family EnvType (m :: * -> *) :: *

class Monad m => MonadReader m where
    ask :: m (EnvType m)
    local :: (EnvType m -> EnvType m) -> m a -> m a

type instance EnvType (Reader r) = r

instance MonadReader (Reader r) where
    ask = R.ask
    local = R.local

type instance EnvType ((->) r) = r

instance MonadReader ((->) r) where
    ask = id
    local f g = g . f

type instance EnvType EnvError = Env

instance MonadReader EnvError where
    ask = E.ask
    local = E.local

withDouble :: (MonadReader m, Num (EnvType m)) => m a -> m a
withDouble = local (* 2)

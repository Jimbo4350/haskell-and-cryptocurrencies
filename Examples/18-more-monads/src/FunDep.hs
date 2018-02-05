{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module FunDep
    ( MonadReader (..)
    , withDouble
    ) where

import           EnvError (Env, EnvError (..))
import qualified EnvError as E
import           Reader   (Reader (..))
import qualified Reader   as R

class Monad m => MonadReader r m | m -> r where
    ask :: m r
    local :: (r -> r) -> m a -> m a

instance MonadReader r (Reader r) where
    ask = R.ask
    local = R.local

instance MonadReader Env EnvError where
    ask = E.ask
    local = E.local

withDouble :: (Num r, MonadReader r m) => m a -> m a
withDouble = local (* 2)

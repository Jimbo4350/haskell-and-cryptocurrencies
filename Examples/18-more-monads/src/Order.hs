{-# LANGUAGE FlexibleContexts #-}

module Order
    ( M1
    , M2
    , test
    , run1
    , run2
    ) where

import           Control.Monad.Except
import           Control.Monad.State

type M1 a = StateT Int (Either String) a -- Int -> Either String (a, Int)

type M2 a = ExceptT String (State Int) a -- Int -> (Either String a, Int)

test :: (MonadState Int m, MonadError String m) => m ()
test = do
    modify (+ 1)
    _ <- throwError "ERROR"
    modify (+ 1)

run1 :: Either String ((), Int) -- Left "ERROR"
run1 = runStateT (test :: M1 ()) 42

run2 :: (Either String (), Int) -- (Left "ERROR", 43)
run2 = runState (runExceptT (test :: M2 ())) 42

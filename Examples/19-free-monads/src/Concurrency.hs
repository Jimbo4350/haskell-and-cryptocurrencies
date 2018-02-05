{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}

module Concurrency where

import Control.Monad
import Free

data ProcessOp :: * -> * where
    Atomically :: IO a -> (a -> r) -> ProcessOp r
    Fork       :: Process () -> r -> ProcessOp r

deriving instance Functor ProcessOp

type Process = Free ProcessOp

atomically :: IO a -> Process a
atomically m = Wrap (Atomically m Return)

fork :: Process () -> Process ()
fork p = Wrap (Fork p (Return ()))

schedule :: [Process ()] -> IO ()
schedule []                           = return ()
schedule (Return _ : ps)              = schedule ps
schedule (Wrap (Atomically m k) : ps) = do
    x <- m
    schedule $ ps ++ [k x]
schedule (Wrap (Fork p1 p2) : ps)     =
    schedule $ ps ++ [p2, p1]

example :: Process ()
example = do
    fork (replicateM_ 5 $ atomically $ putStrLn "Haskell")
    fork (replicateM_ 6 $ atomically $ putStrLn "cryptocurrencies")
    atomically $ putStrLn "2017"

runExample :: IO ()
runExample = schedule [example]

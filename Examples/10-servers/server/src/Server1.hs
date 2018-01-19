module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Network
import System.IO

main :: IO ()
main = do
    s <- listenOn (PortNumber 8765)
    conns <- newTVarIO 0
    forever $ do
        (h, _, _) <- accept s
        forkFinally
            (handleClient h conns)
            (const $ removeClient h conns)

handleClient :: Handle -> TVar Int -> IO ()
handleClient h conns = do
    atomically $ modifyTVar' conns $ \x -> x + 1
    hSetBuffering h LineBuffering
    forever $ do
        _line <- hGetLine h
        count <- readTVarIO conns
        hPrint h count

removeClient :: Handle -> TVar Int -> IO ()
removeClient h conns = do
    atomically $ modifyTVar' conns $ \x -> x - 1
    hClose h

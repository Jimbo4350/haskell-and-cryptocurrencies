module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.List              (delete)
import Network
import System.IO

main :: IO ()
main = do
    s <- listenOn (PortNumber 8765)
    conns <- newTVarIO []
    void $ forkIO $ monitor 0 conns
    forever $ do
        (h, _, _) <- accept s
        forkFinally
            (handleClient h conns)
            (const $ removeClient h conns)

handleClient :: Handle -> TVar [Handle] -> IO ()
handleClient h conns = do
    atomically $ modifyTVar conns (h:)
    hSetBuffering h LineBuffering
    forever $ hGetLine h

removeClient :: Handle -> TVar [Handle] -> IO ()
removeClient h conns = do
    atomically $ modifyTVar' conns $ delete h
    hClose h

monitor :: Int -> TVar [Handle] -> IO ()
monitor count conns = do
    (handles, newCount) <- atomically $ do
        handles <- readTVar conns
        let newCount = length handles
        when (count == newCount) retry
        return (handles, newCount)
    mapM_ (\h -> hPrint h newCount) handles
    monitor newCount conns

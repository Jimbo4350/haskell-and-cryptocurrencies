module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Network
import System.IO

main :: IO ()
main = do
    s <- listenOn (PortNumber 8765)
    conns <- newTVarIO 0
    bchan <- newBroadcastTChanIO
    void $ forkIO $ monitor 0 conns bchan
    forever $ do
        (h, _, _) <- accept s
        forkFinally
            (handleClient h conns bchan)
            (const $ removeClient h conns)

handleClient :: Handle -> TVar Int -> TChan String -> IO ()
handleClient h conns bchan = do
    chan <- atomically $ dupTChan bchan
    atomically $ modifyTVar' conns $ \x -> x + 1
    hSetBuffering h LineBuffering
    void $ input `race` output chan
  where
    input       = forever $ hGetLine h
    output chan = forever $ do
        line <- atomically $ readTChan chan
        hPutStrLn h line

removeClient :: Handle -> TVar Int -> IO ()
removeClient h conns = do
    atomically $ modifyTVar' conns $ \x -> x - 1
    hClose h

monitor :: Int -> TVar Int -> TChan String -> IO ()
monitor count conns bchan = do
    newCount <- atomically $ do
        newCount <- readTVar conns
        when (count == newCount) retry
        return newCount
    atomically $ writeTChan bchan $ show newCount
    monitor newCount conns bchan

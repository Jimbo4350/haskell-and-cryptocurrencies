module Main where

import Control.Concurrent (forkIO)
import Control.Monad      (forever)
import Data.Char          (toUpper)
import Network
import System.IO          (Handle, hSetBuffering, BufferMode (LineBuffering),
                           hGetLine, hPutStrLn)

main :: IO ()
main = do
    let port = PortNumber 8765
    s <- listenOn port
    putStrLn $ "listening on port " ++ show port
    forever $ do
        (h, host, p) <- accept s
        putStrLn $ "client connected: " ++ host ++ ":" ++ show p
        forkIO $ handleClient h

handleClient :: Handle -> IO ()
handleClient h = do
    hSetBuffering h LineBuffering
    forever $ do
        line <- hGetLine h
        hPutStrLn h $ map toUpper line

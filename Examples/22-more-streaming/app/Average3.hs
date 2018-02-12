module Main where

import Fold
import System.Environment (getArgs)

main :: IO ()
main = do
    [size] <- getArgs
    let n  = read size :: Double
    print $ fold average_ [1 .. n]

module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
    [size] <- getArgs
    let n  = read size :: Double
    print $ average [1 .. n]

average :: Fractional a => [a] -> a
average xs = sum xs / fromIntegral (length xs)

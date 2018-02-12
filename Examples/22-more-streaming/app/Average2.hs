{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.List          (foldl')
import System.Environment (getArgs)

main :: IO ()
main = do
    [size] <- getArgs
    let n  = read size :: Double
    print $ average [1 .. n]

sumAndLength :: forall a. Num a => [a] -> (a, Int)
sumAndLength = foldl' op (0, 0)
  where
    op :: (a, Int) -> a -> (a, Int)
    op (!rs, !rl) x = ((+) rs x, (const . (+1)) rl x)

average :: Fractional a => [a] -> a
average xs = case sumAndLength xs of
    (rs, rl) -> rs / fromIntegral rl

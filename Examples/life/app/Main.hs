module Main where

import Control.Lens        ((^.))
import Control.Monad       (forM_, void, foldM)
import Life
import System.Console.ANSI (clearScreen)
import System.Random       (randomIO)

main :: IO ()
main = do
  trues <- randomTrues 5 5
  forM_ (game  trues) $ \b -> do 
      clearScreen
      printBoard 20 20 b
      void getLine

randomTrues :: Int -> Int -> IO [(Int, Int)]
randomTrues x y = foldM f [] [(dx, dy) | dx <- [(-x) .. x], dy <- [(-y) .. y]]

  where

    f ::[(Int, Int)] -> (Int, Int) -> IO [(Int, Int)]
    f ts t = do
        b <- randomIO
        return $ if b then t : ts else ts

printBoard :: Int -> Int -> Board -> IO ()
printBoard x y b =
    forM_ [(-y) .. y] $ \dy -> do
        forM_ [(-x) .. x] $ \dx ->
            putChar $ if b ^. at dx dy then '*' else ' '
        putChar '\n'

module Main where

import Control.Monad (forM)
import Data.List
import System.Environment
import System.Directory
import System.FilePath

-- A naive way to list all files underneath a path.

allFilesRecursively :: FilePath -> IO [FilePath]
allFilesRecursively dir = do
  xs <- getDirectoryContents dir
  ys <- forM xs $ \ x ->
    if "." `isPrefixOf` x
      then return []
      else do
        let f = dir </> x
        b <- doesDirectoryExist f
        if b
          then allFilesRecursively f
          else return [f]
  return (concat ys)

main :: IO ()
main = do
  [dir] <- getArgs
  fs <- allFilesRecursively dir
  mapM_ putStrLn fs

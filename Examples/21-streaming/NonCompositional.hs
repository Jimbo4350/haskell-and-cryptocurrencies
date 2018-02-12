module Main where

import Control.Monad (forM_)
import Data.List
import System.Environment
import System.Directory
import System.FilePath

allFilesRecursively :: FilePath -> IO ()
allFilesRecursively dir = do
  xs <- getDirectoryContents dir
  forM_ xs $ \ x -> do
    if "." `isPrefixOf` x
      then return ()
      else do
        let f = dir </> x
        b <- doesDirectoryExist f
        if b
          then allFilesRecursively f
          else putStrLn f

main :: IO ()
main = do
  [dir] <- getArgs
  allFilesRecursively dir

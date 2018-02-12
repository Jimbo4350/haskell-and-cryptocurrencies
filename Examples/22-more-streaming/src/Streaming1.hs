module Streaming1 where

import           Stream    (Stream)
import qualified Stream    as S
import           System.IO (withFile, IOMode (..), hGetContents)

brokenReadFile :: FilePath -> IO String
brokenReadFile f = withFile f ReadMode hGetContents

catFiles :: [FilePath] -> Stream String IO ()
catFiles = S.mapM brokenReadFile . S.each

test :: Stream String IO ()
test = catFiles (replicate 2000 "stack.yaml")

test1 :: IO ()
test1 = S.stdoutLn test

test2 :: IO String
test2 = concat <$> S.toList test

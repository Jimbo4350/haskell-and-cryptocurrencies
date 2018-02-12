module Naive where

catFiles :: [FilePath] -> IO String
catFiles = fmap concat . mapM readFile

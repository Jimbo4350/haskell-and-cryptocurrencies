module Streaming2 where

import           Prelude   hiding (readFile)
import           Stream    (Stream)
import qualified Stream    as S
import           System.IO (IOMode (..), Handle, hIsEOF, hGetLine, openFile, hClose)

fromHandle :: Handle -> Stream String IO ()
fromHandle h = do
    eof <- S.lift $ hIsEOF h
    if eof
        then return ()
        else do
            l <- S.lift $ hGetLine h
            S.yield l
            fromHandle h

readFile :: FilePath -> Stream String IO ()
readFile f = do
    h <- S.lift $ openFile f ReadMode
    fromHandle h
    S.lift $ putStrLn "closing!" >> hClose h

catFiles :: [FilePath] -> Stream String IO ()
catFiles fs = S.for (S.each fs) readFile

test :: Stream String IO ()
test = catFiles (replicate 2000 "stack.yaml")

test1 :: IO ()
test1 = S.stdoutLn test

test2 :: IO String
test2 = concat <$> S.toList test

test3 :: IO ()
test3 = S.stdoutLn $ S.take 1 $ readFile "stack.yaml"

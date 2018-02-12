module Resources where

import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.Trans.Resource
import qualified Fold                         as F
import           Prelude                      hiding (readFile)
import           Stream                       (Stream)
import qualified Stream                       as S
import           System.IO                    (Handle, hIsEOF, hGetLine, openFile, 
                                               IOMode (..), hClose)

fromHandle :: MonadIO m => Handle -> Stream String m ()
fromHandle h = do
    eof <- S.lift $ liftIO $ hIsEOF h
    if eof
        then return ()
        else do
            l <- S.lift $ liftIO $ hGetLine h
            S.yield l
            fromHandle h

bracketStream :: MonadResource m 
              => IO c 
              -> (c -> IO ()) 
              -> (c -> Stream b m a)
              -> Stream b m a
bracketStream alloc free body = do
    (key, resource) <- S.lift $ allocate alloc free
    result <- body resource
    S.lift $ release key
    return result

readFile :: MonadResource m => FilePath -> Stream String m ()
readFile f = bracketStream
    (openFile f ReadMode)
    (\h -> putStrLn "closing!" >> hClose h)
    fromHandle

catFiles :: MonadResource m => [FilePath] -> Stream String m ()
catFiles fs = S.for (S.each fs) readFile

test :: MonadResource m => Stream String m ()
test = catFiles (replicate 2000 "stack.yaml")

test1 :: IO ()
test1 = runResourceT $ S.stdoutLn test

test2 :: IO String
test2 = runResourceT $ concat <$> S.toList test

test3 :: IO ()
test3 = runResourceT $ S.stdoutLn $ S.take 1 $ readFile "stack.yaml"

problem :: MonadResource m => [FilePath] -> Stream String m ()
problem fs = S.for (S.each fs) (S.take 1 . readFile)

test4 :: IO ()
test4 = runResourceT $ S.stdoutLn $ problem $ replicate 2000 "stack.yaml"

test5 :: IO Int
test5 = runResourceT $ F.purely F.foldS F.length_ test

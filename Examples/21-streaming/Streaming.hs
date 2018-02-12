module Main where

import Control.Monad (forever)
import Control.Monad.Trans
import Data.List
import Pipes
import Pipes.Prelude hiding (read)
import System.Environment
import System.Directory
import System.FilePath
import qualified Streaming.Prelude as S

directoryContents :: FilePath -> S.Stream (S.Of FilePath) IO ()
directoryContents dir =
  liftIO (getDirectoryContents dir) >>= S.each

allFilesRecursively :: FilePath -> S.Stream (S.Of FilePath) IO ()
allFilesRecursively dir =
  S.for (directoryContents dir) $ \ x ->
    if "." `isPrefixOf` x
      then return ()
      else do
        let f = dir </> x
        b <- liftIO $ doesDirectoryExist f
        if b
          then allFilesRecursively f
          else S.yield f

main :: IO ()
main = do
  [dir] <- getArgs
  S.stdoutLn $ allFilesRecursively dir







-- |Some pipe implementations.
map' :: Monad m => (a -> b) -> Pipe a b m r
map' f = forever $ do
    a <- await
    yield $ f a

take' :: Monad m => Int -> Pipe a a m ()
take' 0 = return ()
take' n = do
    a <- await
    yield a
    take' (n - 1)

readLn' :: (Read a, MonadIO m) => Producer a m ()
readLn' = stdinLn >-> map' read



















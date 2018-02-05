module UnsafeIO
    ( Tree (..)
    , relabelTree
    , anything
    , cast
    ) where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving Show

relabelTree :: Tree a -> IO (Tree (a, Int))
relabelTree t = do
    counter <- newIORef 0
    go counter t
  where
    go :: IORef Int -> Tree a -> IO (Tree (a, Int))
    go counter (Leaf a) = do
        c <- atomicModifyIORef' counter $ \n -> (n + 1, n)
        return $ Leaf (a, c)
    go counter (Node l r) = do
        l' <- go counter l
        r' <- go counter r
        return $ Node l' r'

anything :: IORef a
anything = unsafePerformIO $ newIORef undefined

-- |Casts any type into any other type...
--
-- >>> cast 'a' :: Int
-- 97
-- >>> cast (98 :: Int) :: Char
-- 'b'
-- cast True :: (Char, Char)
-- *** Segmentation fault
cast :: a -> b
cast a = unsafePerformIO $ do
    writeIORef anything a
    readIORef anything

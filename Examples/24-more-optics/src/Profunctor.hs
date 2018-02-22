module Profunctor where

import           Control.Arrow   (Kleisli (..))
import           Data.Maybe
import           Data.Profunctor
import           Data.Tagged     (Tagged (..))
import           Text.Read

-- Int ---- f ----> String ~~~ p ~~~> Int ---- g ----> (Int, Int)


f :: Int -> String
f n = show n ++ show n

g :: Int -> (Int, Int)
g n = (n + 1, n * 2)

a :: (->) String Int
a = length

k :: Kleisli IO String Int
k = Kleisli $ \s -> do
    putStrLn s
    mn <- readMaybe <$> getLine
    return $ fromMaybe 42 mn

t :: Tagged String Int
t = Tagged 42

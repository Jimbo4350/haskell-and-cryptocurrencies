module MontyHall where

import           Control.Monad
import           Control.Monad.Free
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as M
import           System.Random

newtype ProbF a =
    Pick (NonEmpty a)

instance Functor ProbF where
    fmap f (Pick xs) = Pick $ fmap f xs

type Prob a = Free ProbF a

pick :: NonEmpty a -> Prob a
pick xs = Free $ Pick $ fmap return xs

pick' :: [a] -> Prob a
pick' = pick . NE.fromList

die :: Prob Int
die = pick' [1..6]

dice :: Int -> Prob Int
dice n = sum <$> replicateM n die

probIO :: Prob a -> IO a
probIO = foldFree f
  where
    f :: ProbF x -> IO x
    f (Pick xs) = do
        i <- randomRIO (0, NE.length xs - 1)
        return $ xs NE.!! i

prob :: Ord a => Prob a -> Map a Double
prob (Pure a)         = M.singleton a 1
prob (Free (Pick xs)) =
    let ys = prob <$> xs
        l  = fromIntegral $ NE.length ys
    in  (/ l) <$> M.unionsWith (+) (NE.toList ys)



data Price = Car | Goat deriving (Show, Eq, Ord)

montyHall :: Prob Bool
montyHall = do
    doors <- pick' [[Car, Goat, Goat], [Goat, Car, Goat], [Goat, Goat, Car]]
    guess <- pick' [0, 1, 2]
    opens <- pick' [i | i <- [0, 1, 2], i /= guess, doors !! i == Goat]
    let switch = head $ filter (\i -> i /= guess && i /= opens) [0..2]
    return $ doors !! switch == Car

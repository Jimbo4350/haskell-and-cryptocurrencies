{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}

{-|
Module      : Delayed
Description : delayed computations
Copyright   : (c) Lars Bruenjes, 2017
License     : BSD3
Maintainer  : lars.bruenjes@iohk.io
Stability   : experimental
Portability : portable

This module contains sample solutions
for the "Delayed computations" assignment.
-}

module Delayed where

import Control.Applicative (Alternative (..))
import Data.Foldable       (asum)
import Free

-- |The type of /delayed computations/.
data Delayed a = Now a | Later (Delayed a)

-- |A productive infinite loop.
loop :: Delayed a
loop = Later loop

-- |A version of the factorial function.
factorial :: Int -> Delayed Int
factorial = go 1
  where
    go !acc n
      | n <= 0    = Now acc
      | otherwise = Later (go (n * acc) (n - 1))

-- |Extracts a result from a delayed computation if it is guarded by
-- at most the given number of @'Later'@ constructors,
-- and @'Nothing'@ otherwise.
--
-- >>> runDelayed 100 loop
-- Nothing
-- >>> runDelayed 5 (factorial 5)
-- Just 120
-- >>> runDelayed 4 (factorial 5)
-- Nothing
runDelayed :: Int -> Delayed a -> Maybe a
runDelayed _ (Now a)   = Just a
runDelayed n (Later d)
    | n <= 0           = Nothing
    | otherwise        = runDelayed (n - 1) d

instance Functor Delayed where
    fmap :: (a -> b) -> Delayed a -> Delayed b
    fmap f (Now a)   = Now (f a)
    fmap f (Later d) = Later $ fmap f d

instance Applicative Delayed where
    pure :: a -> Delayed a
    pure = Now

    (<*>) :: Delayed (a -> b) -> Delayed a -> Delayed b
    (<*>) (Now f)   = fmap f
    (<*>) (Later d) = Later . (<*>) d

instance Monad Delayed where
    return :: a -> Delayed a
    return = Now

    (>>=) :: Delayed a -> (a -> Delayed b) -> Delayed b
    Now a   >>= cont = cont a
    Later d >>= cont = Later $ d >>= cont

-- |Delays once, then returns @()@.
tick :: Delayed ()
tick = Later (Now ())

-- |Sums the elements in the list,
-- delays the result for a number of steps equal to the length of the list.
--
-- >>> runDelayed 3 $ psum [1,2,3]
-- Just 6
-- >>> runDelayed 2 $ psum [1,2,3]
-- Nothing
-- >>> runDelayed 0 $ psum []
-- Just 0
psum :: [Int] -> Delayed Int
psum xs = sum <$> mapM (\ x -> tick >> return x) xs

-- |The functor whose free monad is isomorphic to @'Delayed'@.
-- Note that this functor is isomorphic to the @Identity@ functor!
newtype DelayedF a = LaterF a
    deriving Functor

-- |The @'Free' 'LaterF'@-version of @'tick'@.
tick' :: Free DelayedF ()
tick' = Wrap $ LaterF $ return ()

-- |One witness of the isomorphism between @'Delayed'@ and @'Free' 'DelayedF'@.
fromDelayed :: Delayed a -> Free DelayedF a
fromDelayed (Now a)   = return a
fromDelayed (Later d) = tick' >> fromDelayed d

-- |The other witness of the isomorphism between @'Delayed'@ and @'Free' 'DelayedF'@.
toDelayed :: Free DelayedF a -> Delayed a
toDelayed (Return a)          = Now a
toDelayed (Wrap (LaterF m)) = Later $ toDelayed m

instance Alternative Delayed where
    empty = loop

    (<|>) = merge

-- |Merges two delayed computations in a fair manner,
-- returning the result of the computation
-- that finishes first.
merge :: Delayed a -> Delayed a -> Delayed a
merge (Now x)   _         = Now x
merge _         (Now x)   = Now x
merge (Later p) (Later q) = Later (merge p q)

-- |Performs @'psum'@ on every integer list and returns the result that
-- can be obtained with as few delays as possible.
--
-- >>> runDelayed 100 $ firstSum [repeat 1, [1,2,3], [4,5], [6,7,8], cycle [5,6]]
-- Just 9
firstSum :: [[Int]] -> Delayed Int
firstSum = asum . map psum

-- |Merges two delayed computations in a left-biased way,
-- running the left computation slightly sooner than the right computation.
biasedMerge :: Delayed a -> Delayed a -> Delayed a
biasedMerge p q = p <|> Later q

-- |Version of @'asum'@ that uses @'biasedMerge'@ instead of @('<|>')@.
biasedASum :: [Delayed a] -> Delayed a
biasedASum = foldr biasedMerge loop

-- |Version of @'firstSum'@ that uses @'biasedMerge'@ instead of @'merge'@.
--
-- >>> runDelayed 200 $ biasedFirstSum (cycle [repeat 1, [1,2,3], [4,5], [6,7,8], cycle [5,6]])
-- Just 6
-- >>> runDelayed 200 $ biasedFirstSum (replicate 100 (repeat 1) ++ [[1]] ++ repeat (repeat 1))
-- Just 1
biasedFirstSum :: [[Int]] -> Delayed Int
biasedFirstSum = biasedASum . map psum

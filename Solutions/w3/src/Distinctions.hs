{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Distinctions
Description : distinguishing values of type (Bool, Bool)
Copyright   : (c) Lars Bruenjes, 2017
License     : BSD3
Maintainer  : lars.bruenjes@iohk.io
Stability   : experimental
Portability : portable

This module demonstrates how to distinguish
between all ten distinct values of type @('Bool', 'Bool')@.
-}

module Distinctions where

import Control.Exception
import Control.Monad
import Text.Printf

-- |The "bottom" unicode character.
bottom :: String
bottom = "\x22A5"

-- |Evaluates an @'Int'@ in the @'IO'@-monad,
-- returning @'Nothing'@ if the @'Int'@ was bottom
-- and @'Just'@ the value otherwise.
-- We are forced to use @'IO'@ if we want to catch bottom
-- and transform it into @'Nothing'@.
--
-- >>> evalIO 7
-- Just 7
-- >>> evalIO undefined
-- Nothing
evalIO :: Int -> IO (Maybe Int)
evalIO n = catch
    (n `seq` return (Just n))
    (\(_ :: SomeException) -> return Nothing)

-- |All distinct values of type @'Bool'@.
--
-- >>> length allBools
-- 3
allBools :: [Bool]
allBools = [True, False, undefined]

-- |All distinct values of type @'Either' 'Bool' 'Bool'@.
--
-- >>> length allEithers
-- 7
allEithers :: [Either Bool Bool]
allEithers = undefined : map Left allBools ++ map Right allBools

-- |All distinct values of type @'Maybe' 'Bool'@.
--
-- >>> length allMaybes
-- 5
allMaybes :: [Maybe Bool]
allMaybes = undefined : Nothing : map Just allBools

-- |All distinct values of type @'Bool' -> 'Bool'@.
--
-- >>> length allFuncs
-- 11
allFuncs :: [Bool -> Bool]
allFuncs = const True : const False : [\b -> if b then x else y | x <- allBools, y <- allBools]

-- |All distinct values of type @'Bool'@, together with a @'String'@-description
-- of each value.
allBools' :: [(Bool, String)]
allBools' = [(True, "T"), (False, "F"), (undefined, bottom)]

-- |All distinct values of type @('Bool', 'Bool')@, together with a
-- @'String'@-description of each value.
allPairs' :: [((Bool, Bool), String)]
allPairs' = (undefined, bottom) : [((b, c), '(' : s ++ "," ++ t ++ ")") | (b, s) <- allBools', (c, t) <- allBools']

-- |A list of functions from @('Bool', 'Bool')@ to @'Int'@ which collectively
-- can distinguish between any two distinct values of type @('Bool', 'Bool')@.
discriminators :: [(Bool, Bool) -> Int]
discriminators = 
    [ \(b, c) -> let m = if b then 0 else 2
                     n = if c then 0 else 1
                 in  m + n
    , \(b, _) -> if b then 0 else 1
    , \(_, c) -> if c then 0 else 1
    , \(_, _) -> 0
    ]

-- |A helper function that, given two values and a list of discriminator
-- functions, either returns @'Just'@ the index of the first discriminator
-- which distinguishes between the values
-- or @'Nothing'@ if all discriminators behave the same on both values.
-- This has to be done in the @'IO'@-monad for the same reason that
-- @'evalIO'@ has to be done in @'IO'@.
--
-- >>> findDiscriminator True False [(\b -> if b then 0 else 1)]
-- Just 0
-- >>> findDiscriminator True False [const 7]
-- Nothing
findDiscriminator :: forall a. a -> a -> [a -> Int] -> IO (Maybe Int)
findDiscriminator x y fs = go $ zip [0..] fs
  where
    go :: [(Int, a -> Int)] -> IO (Maybe Int)
    go [] = return Nothing
    go ((i, g) : gs) = do
        nx <- evalIO $ g x
        ny <- evalIO $ g y
        if nx == ny
            then go gs
            else return $ Just i

-- |First prints the value of each discriminator applied to each value of type
-- @('Bool', 'Bool')@,
-- then prints the index of the first successful discriminator for each pair of
-- such values.
--
-- >>> distinguish
--            0     1     2     3
-- <BLANKLINE>
--     ⊥      ⊥     ⊥     ⊥     ⊥
-- (T,T)      0     0     0     0
-- (T,F)      1     0     1     0
-- (T,⊥)      ⊥     0     ⊥     0
-- (F,T)      2     1     0     0
-- (F,F)      3     1     1     0
-- (F,⊥)      ⊥     1     ⊥     0
-- (⊥,T)      ⊥     ⊥     0     0
-- (⊥,F)      ⊥     ⊥     1     0
-- (⊥,⊥)      ⊥     ⊥     ⊥     0
-- <BLANKLINE>
--            ⊥ (T,T) (T,F) (T,⊥) (F,T) (F,F) (F,⊥) (⊥,T) (⊥,F) (⊥,⊥)
-- <BLANKLINE>
--     ⊥      -     0     0     1     0     0     1     2     2     3
-- (T,T)      0     -     0     0     0     0     0     0     0     0
-- (T,F)      0     0     -     0     0     0     0     0     0     0
-- (T,⊥)      1     0     0     -     0     0     1     1     1     1
-- (F,T)      0     0     0     0     -     0     0     0     0     0
-- (F,F)      0     0     0     0     0     -     0     0     0     0
-- (F,⊥)      1     0     0     1     0     0     -     1     1     1
-- (⊥,T)      2     0     0     1     0     0     1     -     2     2
-- (⊥,F)      2     0     0     1     0     0     1     2     -     2
-- (⊥,⊥)      3     0     0     1     0     0     1     2     2     -

distinguish :: IO ()
distinguish = do
    putStr "      "
    forM_ (zip [(0 :: Int)..] discriminators) $ \(i, _) -> printf "%6d" i
    putStr "\n\n"
    forM_ allPairs' $ \(y, t) -> do
        printf "%5s " t
        forM_ discriminators $ \f -> do
            ny <- evalIO $ f y
            printf "%6s" $ maybe bottom show ny
        putStr "\n"
    putStr "\n"
    putStr "      "
    forM_ allPairs' $ \(_, s) -> printf "%6s" s
    putStr "\n\n" 
    forM_ allPairs' $ \(y, t) -> do
        printf "%5s " t
        forM_ allPairs' $ \(x, _) -> do
            mi <- findDiscriminator x y discriminators
            printf "%6s" $ maybe "-" show mi
        putStr "\n"

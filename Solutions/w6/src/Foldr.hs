{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

{-|
Module      : Foldr
Description : foldr-build fusion
Copyright   : (c) Lars Bruenjes, 2017
License     : BSD3
Maintainer  : lars.bruenjes@iohk.io
Stability   : experimental
Portability : portable

Sample solution for W6.2.
-}

module Foldr where

import Control.Exception (try, SomeException)

-- |Builds a list, given a builder.
--
-- >>> build $ \nil cons -> cons 'x' nil
-- "x"
build :: (forall r. r -> (a -> r -> r) -> r) -> [a]
build builder = builder [] (:)

-- |Builds replicated lists.
replicateBuilder :: Int -> a -> (forall r. r -> (a -> r -> r) -> r)
replicateBuilder n x nil cons =
    let go m = if m <= 0
            then nil
            else cons x $ go $ m - 1
    in  go n

-- |Version of replicate using @`build`@.
--
-- >>> replicate' 3 'x'
-- "xxx"
replicate' :: Int -> a -> [a]
replicate' n x = build $ replicateBuilder n x

-- |Builder for int ranges.
fromToBuilder :: Int -> Int -> (forall r. r -> (Int -> r -> r) -> r)
fromToBuilder a b nil cons =
    let go x = if x > b then nil else cons x (go (x + 1))
    in  go a

-- |For integers @a@ and @b@,
-- @`fromTo` a b@ returns the list @[a, a + 1 .. b]@.
--
-- >>> fromTo 1 5
-- [1,2,3,4,5]
-- >>> fromTo 4 4
-- [4]
-- >>> fromTo 5 4
-- []
--
fromTo :: Int -> Int -> [Int]
fromTo a b = build $ fromToBuilder a b

-- |Tries to evaluate the argument to WHNF.
-- Using this, we can write a doctest for a counter-example to the
-- foldr-build fusion law:
--
-- >>> :set -XRankNTypes
-- >>> let builder = flip seq  :: forall r. r -> (Int -> r -> r) -> r
-- >>> let op      = undefined :: Int -> Int -> Int
-- >>> let e       = 0         :: Int
-- >>> foldr op e (build builder)
-- 0
-- >>> tryEvaluate (builder e op)
-- Nothing
tryEvaluate :: forall a. a -> IO (Maybe a)
tryEvaluate a = do
    ea <- try $ a `seq` return a :: IO (Either SomeException a)
    return $ either (const Nothing) Just ea

-- |Filter written as an instance of @`build`@ and @`foldr`@.
--
-- >>> filter' odd [1..10]
-- [1,3,5,7,9]
filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = build $ \nil cons ->
    let op a r
            | p a       = cons a r
            | otherwise = r
    in  foldr op nil xs

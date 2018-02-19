{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Nested
Description : nested datatypes
Copyright   : (c) Lars Bruenjes, 2017
License     : BSD3
Maintainer  : lars.bruenjes@iohk.io
Stability   : experimental
Portability : portable

This module contains sample solutions
for the Nested datatypes assignment.
-}

module Nested where

import           Control.DeepSeq     (NFData)
import qualified Control.Monad.State as S
import           GHC.Generics        (Generic)
import           Prelude             hiding (reverse)
import           Test.QuickCheck

-- |The (nested) datatype of /perfect trees/.
data Perfect a = Z a | S (Perfect (a, a))
    deriving (Show, Eq, Generic)

-- |An @'Arbitrary'@ instance for perfect trees useful for testing.
instance Arbitrary a => Arbitrary (Perfect a) where

    arbitrary = sized $ genPerfect . blog . succ
      where
        genPerfect :: Arbitrary b => Int -> Gen (Perfect b)
        genPerfect 0 = Z <$> arbitrary
        genPerfect d = S <$> genPerfect (d - 1)

        blog :: Int -> Int
        blog 1 = 0
        blog n = 1 + blog (n `div` 2)

instance NFData a => NFData (Perfect a)

-- |Reverses the order of leaves in a perfect tree.
--
-- >>> reverse $ S $ Z (True, False)
-- S (Z (False,True))
reverse :: Perfect a -> Perfect a
reverse p = go p id
  where
    go :: Perfect b -> (b -> b) -> Perfect b
    go (Z b) f = Z (f b)
    go (S q) f = S (go q f')
      where
        f' (x, y) = (f y, f x)

-- |Given a perfect tree and in index, returns @'Just'@
-- the leaf with that index or @'Nothing'@ if the index
-- is out of range.
--
-- >>> index (S $ S $ Z (('0', '1'), ('2', '3'))) 2
-- Just '2'
-- >>> index (S $ S $ Z (('0', '1'), ('2', '3'))) 4
-- Nothing
index :: forall a. Perfect a -> Int -> Maybe a
index p i = go p i id
  where
    go :: Perfect b -> Int -> (b -> a) -> Maybe a
    go (Z b) 0 f = Just (f b)
    go (Z _) _ _ = Nothing
    go (S q) n f = go q (n `div` 2) f'
      where
        f' (x, y)
            | even n    = f x
            | otherwise = f y

-- |Builds a perfect tree with the specified depth,
-- using the specified supply of elements.
--
-- >>> build 2 "12345"
-- S (S (Z (('1','2'),('3','4'))))
build :: Int -> [a] -> Perfect a
build = S.evalState . buildS

-- |Version of @'build'@ using the @'State'@-monad,
-- where the supplied list of elements is the state.
buildS :: Int -> S.State [a] (Perfect a)
buildS n = buildS' n $ do
    (x : xs) <- S.get
    S.put xs
    return x

-- |A helper function for @'buildS'@ with an additional
-- argument, which says how to extract a @b@ from the supply
-- of @a@'s.
buildS' :: Int -> S.State [a] b -> S.State [a] (Perfect b)
buildS' 0 m = Z <$> m
buildS' n m = S <$> buildS' (n - 1) ((,) <$> m <*> m)

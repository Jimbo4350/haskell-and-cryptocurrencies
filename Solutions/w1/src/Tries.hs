{-|
Module      : Tries
Description : tries
Copyright   : (c) Lars Bruenjes, 2017
License     : BSD3
Maintainer  : lars.bruenjes@iohk.io
Stability   : experimental
Portability : portable

This module implements [Tries](https://en.wikipedia.org/wiki/Trie).
-}

{-# LANGUAGE TupleSections, ScopedTypeVariables, InstanceSigs #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Tries
  ( Trie ()
  , empty
  , null
  , valid
  , insert
  , lookup
  , delete
  , genValidTrie
  , toList
  , keys
  , elems
  , member
  ) where

import           Data.Map        (Map)
import qualified Data.Map        as M
import           Prelude         hiding (null, lookup)
import           Test.QuickCheck

-- |A @'Trie' a b@ is a map with keys of type @[a]@ and values of type @b@.
data Trie a b = Fork (Maybe b) (Map a (Trie a b))
  deriving (Show, Eq)

instance Functor (Trie a) where

  fmap :: (b -> c) -> Trie a b -> Trie a c
  fmap f (Fork mb m) = Fork (f <$> mb) (fmap f <$> m)

instance Foldable (Trie a) where

  foldr :: (b -> r -> r) -> r -> Trie a b -> r
  foldr f c t = foldr f c $ trieToList t
   where
    trieToList (Fork mb m) =
      let xs = concatMap trieToList m
      in  case mb of
        Nothing -> xs
        Just b  -> b : xs

-- |The empty trie.
--
-- >>> length empty
-- 0
empty :: Trie a b
empty = Fork Nothing M.empty

-- |Checks whether a trie is empty.
-- This works for /all/ tries, not just /valid/ ones
-- (and is used in the definition of @`valid`@).
--
-- >>> null empty
-- True
-- >>> null (insert "IOHK" True empty)
-- False
null :: Trie a b -> Bool
null (Fork (Just _) _) = False
null (Fork Nothing m)  = all null m

-- |Checks whether a trie is valid
-- (all tries produced by the public API of this module are valid).
valid :: Trie a b -> Bool
valid (Fork _ m) = all (\t -> valid t && not (null t)) m

-- |Inserts a value for a key into a trie.
insert :: Ord a
       => [a]      -- ^ the key
       -> b        -- ^ the value
       -> Trie a b -- ^ the trie
       -> Trie a b -- ^ a trie with the given value associated to the given key
insert []       b (Fork _ m)  = Fork (Just b) m -- If the key list is empty, we have found the right spot to put the value.
insert (a : as) b (Fork mb m) =                 -- If not...
  let t'  = M.findWithDefault empty a m         -- ...we find the correct sub-trie (or take the empty one),
      t'' = insert as b t'                      -- insert the value recursively
      m'  = M.insert a t'' m                    -- and put the updated sub-trie back.
  in  Fork mb m'

-- |Looks up a key in a trie. Returns @'Just'@ the value or @'Nothing'@.
--
-- >>> lookup "foo" empty
-- Nothing
-- >>> lookup "foo" (insert "foo" 42 empty)
-- Just 42
lookup :: Ord a
       => [a]      -- ^ the key
       -> Trie a b -- ^ the trie
       -> Maybe b  -- ^ @'Just' b@ if @b@ is associated to the given key; otherwise @'Nothing'@
lookup []       (Fork mb _) = mb
lookup (a : as) (Fork _  m) = do
  t <- M.lookup a m
  lookup as t

-- |Deletes a key (and its associated value) from a trie.
--
-- >>> lookup "foo" $ delete "foo" $ insert "foo" 42 empty
-- Nothing
delete :: Ord a
       => [a]      -- ^ the key to delete
       -> Trie a b -- ^ the trie
       -> Trie a b -- ^ the trie with the given key (and its associated value) deleted
delete [] (Fork _ m)          = Fork Nothing m        -- If the key is the empty list, simply delete the value.
delete (a : as) t@(Fork mb m) = case M.lookup a m of
  Nothing -> t                                        -- If the head of the key is not in the dictionary, there's
                                                      -- nothing to delete.
  Just t' ->
    let t'' = delete as t'                            -- Delete recursively.
        m'  = if null t''
          then M.delete a m                           -- If the resulting sub-trie is empty, we remove it from the
                                                      -- dictionary to keep the trie valid.
          else M.insert a t'' m                       -- Otherwise, we update the sub-trie.
    in  Fork mb m'

instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (Trie a b) where

  arbitrary :: Gen (Trie a b)
  arbitrary = sized $ \n -> if n == 0      -- We interpret the size n as maximum number of values
                                           -- stored in the trie.
    then return empty                      -- If the n == 0, the trie must be empty.
    else do
      mb <- arbitrary                      -- Chose a value for the empty key (or not).
      let n' = maybe n (const $ n - 1) mb  -- n' is the maximum number of additional elements.
      as <- arbitrary :: Gen [a]           -- Pick arbitrary key heads.
      m  <- resize n' $ f as               -- resize to n' before picking an arbitrary Map.
      return $ Fork mb m
   where
    f ::[a] -> Gen (Map a (Trie a b))
    f []       = return M.empty
    f (a : as) = sized $ \n -> do
      t <- arbitrary                       -- Recursively pick an arbitrary sub-trie.
      let n' = max 0 (n - length t)        -- update the maximum number of additional elements.
      m <- resize n' (f as)                -- Pick the rest of the dictionary recursively.
      return $ M.insert a t m

  shrink :: Trie a b -> [Trie a b]         -- We want to ensure that all shrinks of a valid trie are valid.
  shrink (Fork mb m) =
    let ms =    [M.insert a t'' m | (a, t') <- M.toList m, t'' <- shrink t', not (null t'')]
                -- Recursively shrink a sub-trie, but don't let it become empty.
             ++ [M.delete a m | a <- M.keys m]
                -- Delete a sub-trie from the Map.
    in  case mb of
          Nothing -> [Fork Nothing m' | m' <- ms]        -- If no value is stored for the empty key, we just take what we got.
          Just _  ->    [Fork mb m' | m' <- ms]          -- If there is a value, we can additionally delete that value.
                     ++ [Fork Nothing m' | m' <- m : ms] -- In that case, we can optionally keep the Map as is.

-- | A QuickCheck generator for /@'valid'@/ tries.
genValidTrie :: forall a b. (Ord a, Arbitrary a, Arbitrary b) => Gen (Trie a b)
genValidTrie = sized $ \n -> case n of
  0 -> return empty
  1 -> oneof
    [ return empty
    , arbitrary >>= \b -> return $ Fork (Just b) M.empty
    ]
  _ -> do
    mb <- arbitrary
    let n' = maybe n (const $ n - 1) mb
    as <- arbitrary :: Gen [a]
    m  <- resize n' $ f as
    return $ Fork mb m
 where
  f ::[a] -> Gen (Map a (Trie a b))
  f []       = return M.empty
  f (a : as) = sized $ \n -> do
    t <- genValidTrie
    let l = length t
    let n' = max 0 (n - l)
    m <- resize n' (f as)
    return $ if l == 0 then m else M.insert a t m

-- | Converts a trie to a list of key-value pairs.
--
-- >>> toList $ insert "foo" 42 $ insert "bar" 17 empty
-- [("bar",17),("foo",42)]
toList :: Trie a b -> [([a], b)]
toList (Fork mb m) =
  let xs = [(a : as, b) | (a, t) <- M.toList m, (as, b) <- toList t]
  in  case mb of
        Nothing -> xs
        Just b  -> ([], b) : xs

-- | Returns the keys stored in a trie.
--
-- >>> keys $ insert "foo" 42 $ insert "bar" 17 empty
-- ["bar","foo"]
keys :: Trie a b -> [[a]]
keys = map fst . toList

-- | Returns the values stored in a trie.
--
-- >>> elems $ insert "foo" 42 $ insert "bar" 17 empty
-- [17,42]
elems :: Trie a b -> [b]
elems = map snd . toList

-- | Checks whether a given key is stored in a trie.
--
-- >>> member "foo" empty
-- False
-- >>> member "foo" $ insert "foo" 42 empty
-- True
member :: Ord a => [a] -> Trie a b -> Bool
member as t = maybe False (const True) $ lookup as t

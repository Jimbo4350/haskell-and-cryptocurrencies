module Tables
    ( Table ()
    , empty
    , insert
    , delete
    , lookup
    , mapValues
    , mapKeys
    , alter
    ) where

import Prelude hiding (lookup)

-- START HERE AFTER reaching the pointer in Datatypes.hs

newtype Table k v = Table [(k, v)]
  deriving (Show)

-- In the following, we first reimplement the functions
-- from the slides, but with the @newtype@-based version
-- of the 'Table' type.

-- Task Tables-1.
--
-- Re-implement 'empty'.

empty :: Table k v
empty = Table []

-- Task Tables-2.
--
-- Re-implement 'insert'.

insert :: k -> v -> Table k v -> Table k v
insert k v (Table kvs) = Table ((k, v) : kvs)

-- Task Tables-3.
--
-- Re-implement 'delete'.

delete :: Eq k => k -> Table k v -> Table k v
delete _ (Table [])              = empty
delete k (Table ((k', v) : kvs)) = case delete k (Table kvs) of
    t@(Table kvs') -> if k == k' then t else Table ((k', v) : kvs')

-- Task Tables-4.
--
-- Re-implement 'lookup'.

lookup :: Eq k => k -> Table k v -> Maybe v
lookup _ (Table [])              = Nothing
lookup k (Table ((k', v) : kvs))
    | k == k'                    = Just v
    | otherwise                  = lookup k (Table kvs)

-- Task Tables-5.
--
-- Implement a map function on the table values.

mapValues :: (v1 -> v2) -> Table k v1 -> Table k v2
mapValues f (Table kvs) = Table (map (\(k, v) -> (k, f v)) kvs)

-- Task Tables-6.
--
-- Implement a map function on the table keys.
--
-- Tricky additional question:
-- Can you identify a potential problem with
-- this function?

mapKeys :: (k1 -> k2) -> Table k1 v -> Table k2 v
mapKeys f (Table kvs) = Table (map (\(k, v) -> (f k, v)) kvs)

-- Task Tables-7.
--
-- Implement a more general table update function.
-- The function 'alter' takes a function and a key.

alter :: Eq k => (Maybe v -> Maybe v) -> k -> Table k v -> Table k v
alter f k t = case f (lookup k t) of
    Nothing -> delete k t
    Just v  -> insert k v t

-- Task Tables-8.
--
-- Add an export list to the module, exporting
-- all the functions, and the 'Table' type, but
-- no constructors. The syntax
--
--   Table()
--
-- can be used in the export list to export a
-- datatype or newtype without any of its
-- constructors.

-- GO TO Transactions.hs

module Merkle.Path
    ( MerklePath (..)
    , merklePath
    , checkPath
    ) where

import Data.Binary          (Binary, encode)
import Data.ByteString.Lazy (append)
import Data.List            (foldl')
import Merkle.Core
import Merkle.Hash

data MerklePath a = MerklePath a [Either Hash Hash]
    deriving Show

merklePath :: MerkleTree a -> Int -> Maybe (MerklePath a)
merklePath (Leaf _ a)      i
    | i == 0                 = Just $ MerklePath a []
    | otherwise              = Nothing
merklePath (Node n _ l r)  i
    | i < 2^(n-1)            = do
        MerklePath a ps <- merklePath l i
        return $ MerklePath a $ Right (treeHash r) : ps
    | otherwise              = do
        MerklePath a ps <- merklePath r (i - 2^(n-1))
        return $ MerklePath a $ Left (treeHash l) : ps
merklePath (Partial n _ l) i
    | i < 2^(n-1)            = do
        MerklePath a ps <- merklePath l i
        return $ MerklePath a $ Right (treeHash l) : ps
    | otherwise              = Nothing

checkPath :: Binary a => MerklePath a -> Hash
checkPath (MerklePath a ps) = go $ reverse ps
  where
    go :: [Either Hash Hash] -> Hash
    go = foldl' f $ hash $ encode a

    f :: Hash -> Either Hash Hash -> Hash
    f h (Left l)  = hash $ hashToByteString l `append` hashToByteString h
    f h (Right r) = hash $ hashToByteString h `append` hashToByteString r

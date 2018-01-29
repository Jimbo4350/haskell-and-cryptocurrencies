module Binary where

import           Data.Binary
import           Data.Binary.Get      (ByteOffset)
import qualified Data.ByteString.Lazy as L

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving Show

instance Binary a => Binary (Tree a) where

    put (Leaf a)   = putWord8 0 >> put a
    put (Node l r) = putWord8 1 >> put l >> put r

    get = do
        tag <- getWord8
        case tag of
            0 -> Leaf <$> get
            1 -> Node <$> get <*> get
            _ -> fail "not a tree"

type Result = Either (L.ByteString, ByteOffset, String) (L.ByteString, ByteOffset, Tree Char)

test1, test2 :: Result
test1 = decodeOrFail $ encode $ Node (Leaf 'x') (Leaf 'y')
test2 = decodeOrFail $ encode 'x'

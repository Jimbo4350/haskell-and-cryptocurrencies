module Merkle.Hash
    ( Hash
    , hash
    , hashToByteString
    ) where

import Crypto.Hash          hiding (hash)
import Data.ByteArray       (unpack)
import Data.ByteString.Lazy (ByteString, pack)

type Hash = Digest SHA256

hash :: ByteString -> Hash
hash = hashlazy

hashToByteString :: Hash -> ByteString
hashToByteString = pack . unpack

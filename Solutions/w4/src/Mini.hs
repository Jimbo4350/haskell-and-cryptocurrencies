{-# OPTIONS_HADDOCK ignore-exports #-}

{-|
Module      : Mini
Description : mini private keys & wallet import format
Copyright   : (c) Lars Bruenjes, 2017
License     : BSD3
Maintainer  : lars.bruenjes@iohk.io
Stability   : experimental
Portability : portable

This module contains sample solutions
for the Mini private keys assignment.
-}

module Mini where

import           Control.Monad          (guard)
import           Crypto.Hash            (Digest, SHA256, hash)
import qualified Data.ByteArray         as A
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base58 as B58
import qualified Data.ByteString.Char8  as C
import           Data.Maybe             (fromMaybe)
import           Data.Word              (Word8)

-- |Computes the SHA256 hash of the given @'ByteString'@.
sha256 :: ByteString -> ByteString
sha256 bs = let digest = hash bs :: Digest SHA256 in B.pack $ A.unpack digest

-- |Checks whether a mini private key (given as a @'ByteString'@)
-- is valid.
--
-- >>> isValid $ C.pack "S6c56bnXQiBjk9mqSYE7ykVQ7NzrRy"
-- True
isValid :: ByteString -> Bool
isValid bs
    | B.length bs /= 30 = False
    | otherwise         =
        let bs'     = bs `C.snoc` '?'
            d       = B.head $ sha256 bs' -- Taking the head is okay, because sha256 always produces 256 bits.
        in  d == 0

-- |Validates a mini private key (given as a @'String'@), returns @'Nothing'@ if the key is invalid,
-- otherwise @'Just'@ the private key.
--
-- >>> validate "S6c56bnXQiBjk9mqSYE7ykVQ7NzrRy"
-- Just "4c7a9640c72dc2099f23715d0c8a0d8a35f8906e3cab61dd3f78b67bf887c9ab"
validate :: String -> Maybe String
validate mini =
    let bs = C.pack mini
    in  if isValid bs
            then Just $ C.unpack $ B16.encode $ sha256 bs
            else Nothing

-- |A helper function to print a @'Maybe' 'String'@ to the screen,
-- writing @INVALID@ in case of @'Nothing'@.
putMaybeString :: Maybe String -> IO ()
putMaybeString = putStrLn . fromMaybe "INVALID"

-- |Reads a mini private key from standard input.
-- Writes the corresponding private key to standard output
-- if the mini private key was valid,
-- writes @INVALID@ otherwise.
validateIO :: IO ()
validateIO = do
    s <- getLine
    putMaybeString $ validate s

-- |The targeted network.
data Net = Main | Test
    deriving (Show, Read, Eq)

-- |Gets the prefix for the specified @'Net'@ in the extended private key.
prefix :: Net -> Word8
prefix Main = 0x80
prefix Test = 0xef

-- |The public key mode.
data Mode = Compressed | Uncompressed
    deriving (Show, Read, Eq)

-- |The suffix to append to a private key in case of a /compressed/ public key.
compressedSuffix :: Word8
compressedSuffix = 0x01

-- |Given a mode and a private key, adds the suffix corresponding to the
-- @'Mode'@ (i.e. does nothing for @'Uncompressed'@ and appends a @0x01@
-- for @'Compressed'@).
addSuffix :: Mode -> ByteString -> ByteString
addSuffix Compressed bs   = B.snoc bs compressedSuffix
addSuffix Uncompressed bs = bs

-- |Given a mode and a private key, tries to strip the suffix corresponding to the
-- @'Mode'@ (i.e. does nothing for @'Uncompressed'@ and strips a @0x01@
-- for @'Compressed'@).
stripSuffix :: Mode -> ByteString -> Maybe ByteString
stripSuffix Compressed bs   = case B.unsnoc bs of
    Nothing                     -> Nothing
    Just (bs', b)
        | b == compressedSuffix -> Just bs'
        | otherwise             -> Nothing
stripSuffix Uncompressed bs = Just bs

-- |A type representing to possible configurations for conversion to- and from
-- wallet import format.
data WalletParams = WalletParams Net Mode
    deriving (Show, Eq)

-- |Tries to convert a private key to wallet input format.
--
-- >>> toWallet (WalletParams Main Uncompressed) "0c28fca386c7a227600b2fe50b7cae11ec86d3bf1fbe471be89827e19d72aa1d"
-- Just "5HueCGU8rMjxEXxiPuD5BDku4MkFqeZyd4dZ1jvhTVqvbTLvyTJ"
toWallet :: WalletParams -> String -> Maybe String
toWallet (WalletParams net mode) s = do
    let (pk, r) = B16.decode $ C.pack s
    guard $ B.null r
    let pk'  = addSuffix mode $ B.cons (prefix net) pk
        h1   = sha256 pk'
        h2   = sha256 h1
        cs   = B.take 4 h2
        pk'' = pk' `B.append` cs
        w    = B58.encodeBase58 B58.bitcoinAlphabet pk''
    return $ C.unpack w

-- |Tries to convert from wallet input format to a private key.
--
-- >>> fromWallet (WalletParams Main Uncompressed) "5HueCGU8rMjxEXxiPuD5BDku4MkFqeZyd4dZ1jvhTVqvbTLvyTJ"
-- Just "0c28fca386c7a227600b2fe50b7cae11ec86d3bf1fbe471be89827e19d72aa1d"
fromWallet :: WalletParams -> String -> Maybe String
fromWallet (WalletParams net mode) s = do
    bs <- B58.decodeBase58 B58.bitcoinAlphabet $ C.pack s
    let l = B.length bs
    guard $ l >= 5
    guard $ B.head bs == prefix net
    let (bs', cs) = B.splitAt (l - 4) bs
        h1        = sha256 bs'
        h2        = sha256 h1
        cs'       = B.take 4 h2
    guard $ cs == cs'
    let pk = B.tail bs'
    pk' <- stripSuffix mode pk
    return $ C.unpack $ B16.encode pk'

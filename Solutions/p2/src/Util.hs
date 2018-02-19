{-# LANGUAGE ScopedTypeVariables #-}

module Util
    ( showBS
    , withTimeout
    , TimeoutResult (..)
    , checksum
    , getTimestamp
    , getOwnHost
    , tryConnectTo
    ) where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (race)
import           Control.Exception        (SomeException, try)
import           Crypto.Hash              (hash, SHA256, Digest)
import           Data.Binary              (Binary (..))
import           Data.Binary.Get          (runGet)
import qualified Data.ByteArray           as A
import qualified Data.ByteString.Base16   as B16
import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString          as B
import           Data.Time.Clock.POSIX    (getPOSIXTime)
import qualified Network                  as N
import qualified Network.Info             as NI
import           System.IO                (Handle, hSetBuffering, BufferMode (NoBuffering))
import           Text.Read                (readMaybe)
import           Types

showBS :: B.ByteString -> String
showBS = show . B16.encode

data TimeoutResult a =
      Result a
    | Timeout
    | Exception SomeException
    deriving Show

withTimeout :: forall a. Int -> IO a -> IO (TimeoutResult a)
withTimeout ms x = do
    r <- try $ race (threadDelay ms) x :: IO (Either SomeException (Either () a))
    return $ case r of
        Left e          -> Exception e
        Right (Left ()) -> Timeout
        Right (Right a) -> Result a

checksum :: Payload -> Checksum
checksum = runGet get . LB.fromStrict .B.take 4 . sha256 . sha256 . getPL

sha256 :: B.ByteString -> B.ByteString
sha256 bs =
    let digest = hash bs :: Digest SHA256
    in  B.pack $ A.unpack digest

getTimestamp :: IO Timestamp
getTimestamp = (Timestamp . round) <$> getPOSIXTime

getOwnHost :: IO (Maybe Host)
getOwnHost = do
    xs <- NI.getNetworkInterfaces
    return $ case filter (\x -> NI.mac x /= NI.MAC 0 0 0 0 0 0) xs of
        []      -> Nothing
        (x : _) -> fromIPv6 <$> readMaybe (show $ NI.ipv6 x)

tryConnectTo :: Int -> Port -> Host -> IO (Maybe Handle)
tryConnectTo timeout (Port p) addr = do
    let port = N.PortNumber (fromIntegral p)
        host = show addr
    r <- withTimeout timeout $ N.connectTo host port
    case r of
        Result h    -> hSetBuffering h NoBuffering >> return (Just h)
        Timeout     -> return Nothing
        Exception _ -> return Nothing

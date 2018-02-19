{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types
    ( Net (..)
    , mainnet
    , testnet
    , Checksum
    , Timestamp (..)
    , TimestampSmall (..)
    , Command
    , Magic
    , Length (..)
    , Payload (..)
    , payloadLength
    , putPayload
    , getPayload
    , Host
    , fromIPv4
    , fromIPv6
    , Service (..)
    , Services
    , services
    , Port (..)
    , NetworkAddress (..)
    , NetworkAddressTime (..)
    , Version (..)
    , Height (..)
    , Nonce
    , mkNonce
    , VarInt (..)
    , Binary' (..)
    , VarLength (..)
    , ASCIIString
    , VarString
    , Flag (..)
    ) where

import           Control.Monad          (replicateM)
import           Data.Binary            (Binary (..))
import qualified Data.Binary.Get        as BG
import qualified Data.Binary.Put        as BP
import           Data.Bits              (setBit, testBit)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as C8
import qualified Data.ByteString.Lazy   as LB
import           Data.Int               (Int32)
import qualified Data.IP                as IP
import           Data.List              (nub, sort, foldl')
import           Data.String            (IsString (..))
import           Data.Time.Clock        ()
import           Data.Time.Clock.POSIX  (posixSecondsToUTCTime)
import           Data.Word              (Word8, Word16, Word32, Word64)
import           System.Random          (randomIO)

data Net = Net
    { netPort  :: !Port
    , netMagic :: !Magic
    }
    deriving Show

mainnet, testnet :: Net
mainnet = Net
    { netPort  = Port 8333
    , netMagic = Magic 0xD9B4BEF9
    }
testnet = Net
    { netPort = Port 18333
    , netMagic = Magic 0x0709110B
    }

newtype Checksum = Checksum Word32
    deriving Eq

instance Show Checksum where
    show = C8.unpack . B16.encode . LB.toStrict . BP.runPut . put

instance Binary Checksum where
    put (Checksum w) = BP.putWord32le w
    get = Checksum <$> BG.getWord32le

newtype Timestamp = Timestamp {getTS :: Word64}

instance Show Timestamp where
    show = show . posixSecondsToUTCTime . fromIntegral . getTS

instance Binary Timestamp where
    put (Timestamp w) = BP.putWord64le w
    get = Timestamp <$> BG.getWord64le

newtype TimestampSmall = TimestampSmall {getTSS :: Word32}

instance Show TimestampSmall where
    show = show . posixSecondsToUTCTime . fromIntegral . getTSS

instance Binary TimestampSmall where
    put (TimestampSmall w) = BP.putWord32le w
    get = TimestampSmall <$> BG.getWord32le

newtype Command = Command [Word8]
    deriving Eq

instance IsString Command where
    fromString s = Command $ take 12 $ B.unpack (C8.pack s) ++ repeat 0

instance Show Command where
    show (Command bs) = C8.unpack $ B.pack $ takeWhile (/= 0) bs

instance Binary Command where
    put (Command bs) = mapM_ BP.putWord8 bs
    get = (Command . B.unpack) <$> BG.getByteString 12

newtype Magic = Magic Word32

instance Show Magic where
    show = C8.unpack . B16.encode . B.reverse . LB.toStrict . BP.runPut . put

instance Binary Magic where
    put (Magic w) = BP.putWord32le w
    get = Magic <$> BG.getWord32le

newtype Length = Length {getLength :: Word32}

instance Show Length where
    show (Length w) = show w

instance Binary Length where
    put (Length w) = BP.putWord32le w
    get = Length <$> BG.getWord32le

newtype Payload = Payload {getPL :: B.ByteString}
    deriving Eq

instance Show Payload where
    show = C8.unpack . B16.encode . getPL

payloadLength :: Payload -> Length
payloadLength = Length . fromIntegral . B.length . getPL

putPayload :: Payload -> BP.Put
putPayload = BP.putByteString . getPL

getPayload :: Length -> BG.Get Payload
getPayload (Length w) = Payload <$> BG.getByteString (fromIntegral w)

newtype Host = Host IP.IPv6
    deriving Eq

instance Show Host where
    show (Host ip) = show ip

instance Binary Host where
    put (Host ip) = mapM_ (BP.putWord16be . fromIntegral) $ IP.fromIPv6 ip
    get = do
        ws <- replicateM 8  BG.getWord16be
        return $ fromIPv6 $ IP.toIPv6 $ map fromIntegral ws

fromIPv6 :: IP.IPv6 -> Host
fromIPv6 = Host

fromIPv4 :: IP.IPv4 -> Host
fromIPv4 ip =
    let [a, b, c, d] = IP.fromIPv4 ip
        ab = a * 256 + b
        cd = c * 256 + d
    in  Host $ IP.toIPv6 [0, 0, 0, 0, 0, 65535, ab, cd]

data Service = NODE_NETWORK | NODE_GETUTXO | NODE_BLOOM | NODE_WITNESS | NODE_XTHIN
    deriving (Show, Eq, Ord, Enum, Bounded)

serviceBit :: Service -> Int
serviceBit NODE_NETWORK = 0
serviceBit NODE_GETUTXO = 1
serviceBit NODE_BLOOM   = 2
serviceBit NODE_WITNESS = 4
serviceBit NODE_XTHIN   = 8

newtype Services = Services [Service]

services :: [Service] -> Services
services = Services . sort . nub

instance Show Services where
    show (Services ss) = show ss

instance Binary Services where
    put (Services ss) = BP.putWord64le $ foldl' (\acc s -> setBit acc (serviceBit s)) 0 ss
    get = do
        w <- BG.getWord64le
        let ss = [s | s <- [minBound .. maxBound], testBit w (serviceBit s)]
        return $ services ss

newtype Port = Port Word16

instance Show Port where
    show (Port w) = show w

instance Binary Port where
    put (Port w) = BP.putWord16be w
    get = Port <$> BG.getWord16be

data NetworkAddress = NetworkAddress
    { naServices :: !Services
    , naHost     :: !Host
    , naPort     :: !Port
    } deriving Show

instance Binary NetworkAddress where
    put NetworkAddress{..} = put naServices >> put naHost >> put naPort
    get = NetworkAddress <$> get <*> get <*> get

data NetworkAddressTime = NetworkAddressTime
    { natTimestamp :: !TimestampSmall
    , natServices  :: !Services
    , natHost      :: !Host
    , natPort      :: !Port
    } deriving Show

instance Binary NetworkAddressTime where
    put NetworkAddressTime{..} = put natTimestamp >> put natServices >> put natHost >> put natPort
    get = NetworkAddressTime <$> get <*> get <*> get <*> get

newtype Version = Version Int32

instance Show Version where
    show (Version v) = show v

instance Binary Version where
    put (Version v) = BP.putInt32le v
    get = Version <$> BG.getInt32le

newtype Height = Height Int32

instance Show Height where
    show (Height h) = show h

instance Binary Height where
    put (Height h) = BP.putInt32le h
    get = Height <$> BG.getInt32le

newtype Nonce = Nonce Word64
    deriving Eq

instance Show Nonce where
    show = C8.unpack . B16.encode . LB.toStrict . BP.runPut . put

instance Binary Nonce where
    put (Nonce w) = BP.putWord64le w
    get = Nonce <$> BG.getWord64le

mkNonce :: IO Nonce
mkNonce = Nonce <$> randomIO

newtype VarInt = VarInt Word64

instance Show VarInt where
    show (VarInt w) = show w

instance Binary VarInt where
    put (VarInt w)
        | w <  0xFD       = BP.putWord8 $ fromIntegral w
        | w <= 0xFFFF     = BP.putWord8 0xFD >> BP.putWord16le (fromIntegral w)
        | w <= 0xFFFFFFFF = BP.putWord8 0xFE >> BP.putWord32le (fromIntegral w)
        | otherwise       = BP.putWord8 0xFF >> BP.putWord64le w
    get = do
        b <- BG.getWord8
        case b of
            0xFD -> (VarInt . fromIntegral) <$> BG.getWord16le
            0xFE -> (VarInt . fromIntegral) <$> BG.getWord32le
            0xFF -> VarInt                  <$> BG.getWord64le
            _    -> return $ VarInt $ fromIntegral b

class Show a => Binary' a where
   put' :: a -> LB.ByteString
   get' :: LB.ByteString -> Maybe a

newtype VarLength a = VarLength a

instance Show a => Show (VarLength a) where
    show (VarLength a) = show a

instance Binary' a => Binary (VarLength a) where
    put (VarLength a) = do
        let bs = put' a
            l  = VarInt $ fromIntegral $ LB.length bs
        put l
        BP.putLazyByteString bs
    get = do
        VarInt l <- get
        bs <- BG.getLazyByteString (fromIntegral l)
        case get' bs of
            Nothing -> fail "illegal VarLength"
            Just a  -> return $ VarLength a

newtype ASCIIString = ASCIIString B.ByteString

instance IsString ASCIIString where
    fromString = ASCIIString . C8.pack

instance Show ASCIIString where
    show (ASCIIString bs) = C8.unpack bs

instance Binary' ASCIIString where
    put' (ASCIIString bs) = LB.fromStrict bs
    get' = Just . ASCIIString . LB.toStrict

newtype VarString = VarString (VarLength ASCIIString)

instance IsString VarString where
    fromString = VarString . VarLength . fromString

instance Show VarString where
    show (VarString v) = show v

instance Binary VarString where
    put (VarString v) = put v
    get = VarString <$> get

newtype Flag = Flag Bool

instance Show Flag where
    show (Flag b) = show b

instance Binary Flag where
    put (Flag b) = BP.putWord8 $ if b then 0x01 else 0x00
    get = do
        w <- BG.getWord8
        return $ Flag $ w == 0x01

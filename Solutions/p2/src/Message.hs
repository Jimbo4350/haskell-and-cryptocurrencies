{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Message
    ( RawMessage
    , Message (..)
    , toRawMessage
    , fromRawMessage
    , hTryPutMessage
    , hTryGetMessage
    ) where

import           Control.Exception    (try, SomeException)
import           Control.Monad        (when)
import           Data.Binary          (Binary (..))
import           Data.Binary.Get      (runGet, runGetOrFail)
import           Data.Binary.Put      (runPut)
import qualified Data.ByteString.Lazy as LB
import           Data.Maybe           (fromMaybe)
import           Payloads
import           System.IO            (Handle)
import           Types
import           Util

-- |Generic message type with arbitrary commands and payloads.
data RawMessage = RawMessage
    { msgMagic   :: !Magic
    , msgCommand :: !Command
    , msgPayload :: !Payload
    } deriving Show

instance Binary RawMessage where
    put RawMessage{..} = do
        put msgMagic
        put msgCommand
        put (payloadLength msgPayload)
        put (checksum msgPayload)
        putPayload msgPayload
    get = do
        msgMagic   <- get
        msgCommand <- get
        len        <- get
        chk        <- get
        msgPayload <- getPayload len
        when (chk /= checksum msgPayload) $ fail "checksum error"
        return RawMessage{..}

-- |Typed message, but there is also a fallback to @'RawMessage'@ for unknown
-- message types.
data Message where
    MsgVersion :: Magic -> VersionPayload -> Message
    MsgVerack  :: Magic -> Message
    MsgPing    :: Magic -> Nonce -> Message
    MsgPong    :: Magic -> Nonce -> Message
    MsgAddr    :: Magic -> AddrPayload -> Message
    MsgRaw     :: RawMessage -> Message
    deriving Show

toRawMessage :: Message -> RawMessage
toRawMessage (MsgVersion mgc vp) = RawMessage mgc "version" $ toPayload vp
toRawMessage (MsgVerack mgc)     = RawMessage mgc "vereack" $ Payload ""
toRawMessage (MsgPing mgc n)     = RawMessage mgc "ping" $ toPayload n
toRawMessage (MsgPong mgc n)     = RawMessage mgc "pong" $ toPayload n
toRawMessage (MsgAddr mgc a)     = RawMessage mgc "addr" $ toPayload a
toRawMessage (MsgRaw raw)        = raw

-- |Converts an arbitrary instance of a serializable type into a @'Payload'@.
toPayload :: Binary a => a -> Payload
toPayload = Payload . LB.toStrict . runPut . put

fromRawMessage :: RawMessage -> Message
fromRawMessage = dispatches
    [ Dispatch "version" MsgVersion
    , Dispatch "verack" (\mgc () -> MsgVerack mgc) -- no payload, but that is handled by the Binary instance of ()
    , Dispatch "ping" MsgPing
    , Dispatch "pong" MsgPong
    , Dispatch "addr" MsgAddr
    ]

tryGet :: Binary a => Payload -> Maybe a
tryGet p = case runGetOrFail get $ LB.fromStrict $ getPL p of
    Right ("", _, a) -> Just a
    _                -> Nothing

data Dispatch where
    Dispatch :: Binary a => Command -> (Magic -> a -> Message) -> Dispatch -- using existential type to handle different payloads

dispatch :: Dispatch -> RawMessage -> Maybe Message
dispatch d RawMessage{..} = case d of
    Dispatch cmd toMsg
        | msgCommand == cmd -> case tryGet msgPayload of
            Just a  -> Just (toMsg msgMagic a)
            Nothing -> Nothing
        | otherwise         -> Nothing

dispatches :: [Dispatch] -> RawMessage -> Message
dispatches []       r = MsgRaw r
dispatches (d : ds) r = fromMaybe (dispatches ds r) (dispatch d r)

hTryPutMessage :: Handle -> Message -> IO Bool
hTryPutMessage h msg = do
    putStrLn $ "sending " ++ show msg
    let raw = toRawMessage msg
        bs  = runPut $ put raw
    e <- try $ LB.hPut h bs :: IO (Either SomeException ())
    return $ case e of
        Left  _  -> False
        Right () -> True

hTryGetMessage :: Handle -> Int -> IO (Maybe Message)
hTryGetMessage h timeout = do
    r <- withTimeout timeout $ do
        bs <- LB.hGet h 24
        let len = fromIntegral $ getLength $ runGet get (LB.take 4 $ LB.drop 16 bs)
        cs <- LB.hGet h len
        let raw = runGet get $ bs `LB.append` cs
        return $ fromRawMessage raw
    case r of
        Result msg -> putStrLn ("received " ++ show msg) >> return (Just msg)
        _          -> return Nothing

{-# LANGUAGE OverloadedStrings #-}

module Client where

import Control.Monad (void)
import Message
import Payloads
import Types
import Util

nodes :: Int -> Net -> [Host] -> IO Bool
nodes timeout net = go False
  where
    go b []         = return b
    go b (ip : ips) = do
        c <- node timeout ip net
        go (b || c) ips

node :: Int -> Host -> Net -> IO Bool
node timeout host net = do
    putStrLn $ "connecting to host " ++ show host ++ ", port " ++ show (netPort net)
    mh <- tryConnectTo timeout (netPort net) host
    case mh of
        Nothing -> return False
        Just h  -> do
            Just msg <- tryMkVersionMessage net host
            _ <- hTryPutMessage h msg
            go h False
  where
    go h b = do
        m <- hTryGetMessage h timeout
        case m of
            Nothing  -> return b
            Just (MsgVersion mgc _) -> void (hTryPutMessage h $ MsgVerack mgc) >> go h True
            Just (MsgPing mgc n)    -> void (hTryPutMessage h $ MsgPong mgc n) >> go h True
            Just _                  -> go h True

headerLength :: Int
headerLength = 24

tryMkVersionMessage :: Net -> Host -> IO (Maybe Message)
tryMkVersionMessage net host = do
    m <-mkVersionPayload
        (netPort net)
        (Version 70015)
        (services [])
        host
        "IOHK HC2017 P2"
        (Height 0)
        (Flag False)
    return $ case m of
        Nothing      -> Nothing
        Just payload -> Just $ MsgVersion (netMagic net) payload

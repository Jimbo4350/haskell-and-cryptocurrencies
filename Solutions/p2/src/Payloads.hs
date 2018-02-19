{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Payloads
    ( VersionPayload (..)
    , mkVersionPayload
    , AddrPayload (..)
    ) where

import Control.Monad (replicateM)
import Data.Binary   (Binary (..))
import Types
import Util

data VersionPayload = VersionPayload
    { vpVersion     :: !Version
    , vpServices    :: !Services
    , vpTimestamp   :: !Timestamp
    , vpReceiver    :: !NetworkAddress
    , vpSender      :: !NetworkAddress
    , vpNonce       :: !Nonce
    , vpAgent       :: !VarString
    , vpStartHeight :: !Height
    , vpRelay       :: !Flag
    } deriving Show

instance Binary VersionPayload where
    put VersionPayload{..} = do
        put vpVersion
        put vpServices
        put vpTimestamp
        put vpReceiver
        put vpSender
        put vpNonce
        put vpAgent
        put vpStartHeight
        put vpRelay
    get = VersionPayload <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get

mkVersionPayload :: Port
                 -> Version
                 -> Services
                 -> Host
                 -> VarString
                 -> Height
                 -> Flag
                 -> IO (Maybe VersionPayload)
mkVersionPayload port vpVersion vpServices host vpAgent vpStartHeight vpRelay = do
    mh <- getOwnHost
    case mh of
        Nothing      -> return Nothing
        Just ownHost -> do
            vpTimestamp <- getTimestamp
            let vpReceiver = NetworkAddress
                    { naServices = services []
                    , naHost     = host
                    , naPort     = port
                    }
            let vpSender = NetworkAddress
                    { naServices = vpServices
                    , naHost     = ownHost
                    , naPort     = port
                    }
            vpNonce <- mkNonce
            return $ Just VersionPayload{..}

newtype AddrPayload = AddrPayload [NetworkAddressTime]
    deriving Show

instance Binary AddrPayload where
    put (AddrPayload xs) = do
        let l = length xs
        put (VarInt $ fromIntegral l)
        mapM_ put xs
    get = do
        VarInt l <- get
        AddrPayload <$> replicateM (fromIntegral l) get

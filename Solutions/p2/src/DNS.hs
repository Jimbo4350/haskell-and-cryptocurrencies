module DNS where

import Data.ByteString.Char8 (pack)
import Data.List             (nub)
import Network.DNS.Lookup    (lookupA, lookupAAAA)
import Network.DNS.Resolver  (makeResolvSeed, defaultResolvConf, withResolver)
import Types

lookupBitcoinNodes :: IO [Host]
lookupBitcoinNodes = lookupDNSs
    [ "bitseed.xf2.org"
    , "dnsseed.bluematt.me"
    , "seed.bitcoin.sipa.be"
    , "dnsseed.bitcoin.dashjr.org"
    , "seed.bitcoinstats.com"
    ]

lookupDNSs :: [String] -> IO [Host]
lookupDNSs hosts = (nub . concat) <$> mapM lookupDNS hosts

lookupDNS :: String -> IO [Host]
lookupDNS host = do
    let host' = pack host
    rs <- makeResolvSeed defaultResolvConf
    withResolver rs $ \resolver -> do
        ip4s <- toHosts fromIPv4 <$> lookupA resolver host'
        ip6s <- toHosts fromIPv6 <$> lookupAAAA resolver host'
        return $ ip4s ++ ip6s

toHosts :: (b -> Host) -> Either a [b] -> [Host]
toHosts _ (Left _)   = []
toHosts f (Right bs) = map f bs

{-# LANGUAGE OverloadedStrings #-}

import Client (nodes)
import DNS    (lookupBitcoinNodes)
import Types  (mainnet)

main :: IO ()
main = do
    putStrLn "looking up bitcoin nodes..."
    ips <- lookupBitcoinNodes
    putStrLn $ "...found " ++ show (length ips) ++ " node(s)"
    b <- nodes 3000000 mainnet ips
    putStrLn $ if b then "SUCCESS!!!" else "failure :("

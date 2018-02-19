{-# OPTIONS_HADDOCK ignore-exports #-}

{-|
Module      : WalletParamsParser
Description : Commandline parser for wallet import format tools
Copyright   : (c) Lars Bruenjes, 2017
License     : BSD3
Maintainer  : lars.bruenjes@iohk.io
Stability   : experimental
Portability : portable

This module contains command line parsers and helpers
for the tools related to wallet import format.
-}

module WalletParamsParser where

import Data.Semigroup      ((<>))
import Mini 
import Options.Applicative

-- |A commandline parser for @'WalletParams'@ and the main argument.
params :: Parser (WalletParams, String)
params = 
        (\n m s -> (WalletParams n m, s))
    <$> option auto
        (  long "net"
        <> short 'n'
        <> help "the network to use ('Main' or 'Test')"
        <> showDefault
        <> value Main
        <> metavar "NET"
        )
    <*> option auto
        (  long "mode"
        <> short 'm'
        <> help "the public key mode ('Compressed' or 'Uncompressed')"
        <> showDefault
        <> value Uncompressed
        <> metavar "MODE"
        )
    <*> strArgument
        (  help "The input."
        <> metavar "INPUT"
        )

-- |A @'ParserInfo'@ specialized to the @toWallet@ tool.
toWalletParams :: ParserInfo (WalletParams, String)
toWalletParams = info (params <**> helper)
    (  fullDesc
    <> progDesc "Converts private key INPUT to wallet import format, targeting NET and public keys of type MODE."
    <> header "toWallet - converting a private key to wallet import format"
    )

-- |A @'ParserInfo'@ specialized to the @fromWallet@ tool.
fromWalletParams :: ParserInfo (WalletParams, String)
fromWalletParams = info (params <**> helper)
    (  fullDesc
    <> progDesc "Converts wallet import format INPUT to a private key, targeting NET and public keys of type MODE."
    <> header "fromWallet - converting wallet import format to a private key"
    )

-- |Reads a private key and optional parameters from standard input.
-- Writes the corresponding wallet input format to standard output
-- if the private key was in the right format,
-- writes @INVALID@ otherwise.
toWalletIO :: IO ()
toWalletIO = do
    (ps, x) <- execParser toWalletParams
    putMaybeString $ toWallet ps x

-- |Reads a wallet import format and optional parameters from standard input.
-- Writes the corresponding private key to standard output
-- if the wallet import format was valid,
-- writes @INVALID@ otherwise.
fromWalletIO :: IO ()
fromWalletIO = do
    (ps, x) <- execParser fromWalletParams
    putMaybeString $ fromWallet ps x

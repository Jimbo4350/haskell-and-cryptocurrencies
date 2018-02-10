{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Handshake
Description : free-monad example
Copyright   : (c) Lars Brünjes, 2018
Maintainer  : lars.bruenjes@iohk.io
Stability   : experimental
Portability : portable

This module demonstrates free monads on the example of a simple send/receive API.
Two interpretations of the free monad are given, an impure one using networking
and a pure one using the state monad.
-}

module Handshake where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception        hiding (Deadlock)
import Control.Monad
import Control.Monad.Free
import Control.Monad.State
import Data.Char                (toUpper)
import Data.Sequence
import Network
import System.IO

-- |Describes the desired API.
data HandshakeF a =
      Send String a         -- ^send a string
    | Receive (String -> a) -- ^receive a string

instance Functor HandshakeF where
    fmap f (Send s a)  = Send s $ f a
    fmap f (Receive g) = Receive $ f . g

-- |A free monad offering sending and receiving of strings as API.
type Handshake = Free HandshakeF

-- |Send a @'String'@.
send :: String -> Handshake ()
send s = Free $ Send s $ return ()

-- |Receive a @'String'@.
receive :: Handshake String
receive = Free $ Receive return

-- |An example program written in the @'Handshake'@ monad, describing a "server"
-- that "shouts back" everything it receives.
shoutingServer :: Handshake ()
shoutingServer = forever $ do
    s <- receive
    send $ map toUpper s

-- |Another example program written in the @'Handshake'@ monad, describing a
-- "client" that sends the specified @'String'@ and returns the reply.
shoutingClient :: String -> Handshake String
shoutingClient s = do
    send s
    receive

-- |Interprets a @'Handshake'@-program in @'IO'@ by sending and receiving over
-- the specified @'Handle'@.
handshakeIO :: Handle -> Handshake a -> IO a
handshakeIO h = foldFree f
  where
    f :: HandshakeF x -> IO x
    f (Send s a)  = hPutStrLn h s >> return a         -- send by writing to the handle
    f (Receive g) = hGetLine h >>= \s -> return (g s) -- receive by reading from the handle

-- |Tests two @'Handshake'@-programs, a "server" and a "client" in @'IO'@.
testIO :: PortNumber   -- ^port to use
       -> Handshake () -- ^server program
       -> Handshake a  -- ^client program
       -> IO a         -- ^returns the client result
testIO port server client = do
    socket <- listenOn p

    tid    <- forkIO $ do             -- server thread
        (h, _, _) <- accept socket
        hSetBuffering h LineBuffering
        handshakeIO h server          -- interpret server in IO

    a      <- async $ do              -- client thread
        h <- connect
        hSetBuffering h LineBuffering
        handshakeIO h client          -- interpret client in IO

    result <- wait a                  -- wait for client to finish

    killThread tid                    -- clean up
    sClose socket

    return result                     -- return client result
  where
    p :: PortID
    p = PortNumber port

    connect :: IO Handle              -- repeat trying to connect
    connect = do
        eh <- try $ connectTo "127.0.0.1" p :: IO (Either SomeException Handle)
        case eh of
            Right h -> return h
            Left _  -> connect

-- |Uses @'testIO'@ to test @'shoutingServer'@ and @'shoutingClient'@ on
-- the example string @\"Barbados\"@.
--
-- >>> testShoutingIO
-- "BARBADOS"
testShoutingIO :: IO String
testShoutingIO = testIO 8765 shoutingServer $ shoutingClient "Barbados"

-- |Possible results for interpreting two @'Handshake'@-programs via
-- @'handshakeState'@.
data Result a b =
      Both a b -- ^both programs return a result
    | First a  -- ^the first program returns a result, the second is blocked
    | Second b -- ^the second program returns a result, the first is blocked
    | Deadlock -- ^both programs are blocked
    deriving Show

-- |A pure interpretation of @'Handshake'@-programs in a state monad.
-- Given two such programs, send and receive are implemented by adding to the
-- head and taking from the tail of a sequence.
-- The state consists of two sequences, one for communication in each direction.
handshakeState :: forall a b. Handshake a -> Handshake b -> Result a b
handshakeState x y = evalState (f x y) (empty, empty)
  where
    f :: Handshake a -> Handshake b -> State (Seq String, Seq String) (Result a b)
    f (Pure a)              (Pure b)              = return $ Both a b                                              -- both done
    f (Free (Send s x'))    y'                    = do                                                             -- first sending
                                                        (aToB, bToA) <- get
                                                        put (s <| aToB, bToA)
                                                        f x' y'
    f x'                    (Free (Send t y'))    = do                                                             -- second sending
                                                        (aToB, bToA) <- get
                                                        put (aToB, t <| bToA)
                                                        f x' y'
    f x'@(Pure a)           (Free (Receive h))    = do                                                             -- first done, second receiving
                                                        (aToB, bToA) <- get
                                                        case aToB of
                                                            (xs :|> s) -> put (xs, bToA) >> f x' (h s)             -- receive succeeds
                                                            Empty      -> return $ First a                         -- receive blocks forever
    f (Free (Receive g))    y'@(Pure b)           = do                                                             -- second done, first receiving
                                                        (aToB, bToA) <- get
                                                        case bToA of
                                                            (ys :|> t) -> put (aToB, ys) >> f (g t) y'             -- receive succeeds
                                                            Empty      -> return $ Second b                        -- receive blocks forever
    f x'@(Free (Receive g)) y'@(Free (Receive h)) = do                                                             -- both receive
                                                        (aToB, bToA) <- get
                                                        case (aToB, bToA) of
                                                            (xs :|> s, ys :|> t) -> put (xs, ys) >> f (g t) (h s)  -- both succeed
                                                            (Empty, ys :|> t)    -> put (aToB, ys) >> f (g t) y'   -- first succeeds, second blocks
                                                            (xs :|> s, Empty)    -> put (xs, bToA) >> f x' (h s)   -- second succeeds, first blocks
                                                            (Empty, Empty)       -> return Deadlock                -- both block

-- |Uses @'handshakeState'@ to test @'shoutingServer'@ and @'shoutingClient'@ on
-- the example string @\"Barbados\"@.
--
-- >>> testShoutingState
-- Second "BARBADOS"
testShoutingState :: Result () String
testShoutingState = handshakeState shoutingServer $ shoutingClient "Barbados"

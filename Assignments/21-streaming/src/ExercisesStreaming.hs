{-# OPTIONS_GHC -Wno-unused-imports #-}
module ExercisesStreaming where

import           Control.Concurrent  hiding (yield)
import           Control.Monad.State
import           Prelude             (Either (..), String, ($), (.), (<$>))
import qualified Prelude             as P
import           Streaming
import           Streaming.Internal

-- Compare the version from the slides
--
-- data StreamF b m r =
--     Lift (m r)
--   | Yield b r
--   deriving (Functor)
--
-- data Free f a =
--     Return a
--   | Wrap (f (Free f a))
--
-- type Stream b m = Free (StreamF b m)
--
-- and the version from the streaming package:
--
-- data Stream f m r =
--     Step (f (Stream f m r))
--   | Effect (m (Stream f m r))
--   | Return r
--
-- data Of a b = !a :> b
--
-- There is a strong relationship between the two types,
-- in particular between
--
--      Wrap . Lift :: m (Stream b m a) -> Stream b m a
-- and  Effect      :: m (Stream f m r) -> Stream f m r
--
--      \ x -> Wrap . Yield x :: b -> Stream b m a -> Stream b m a
-- and  \ x -> Step . (x :>)  :: a -> Stream (Of a) m r -> Stream (Of a) m r
--
--      Return :: a -> Stream b m a
-- and  Return :: r -> Stream f m r
--
-- The primary difference is that the package gives us
-- extra flexiblity in that it allows to choose an arbitrary
-- functor for f, with Of being the default choice
-- corresponding to our version of streams.

-- In this set of exercises, we will re-implement some basic
-- functions in terms of the internal interface of the streaming
-- library. Most of these functions are implemented in
-- Streaming.Prelude (so you can look for possible solutions
-- there, but are not supposed to use these versions directly).

-- S1. Implement
--
-- yield :: Monad m => a -> Stream (Of a) m ()

-- S2. Implement
--
-- next :: Monad m => Stream (Of a) m r -> m (Either r (a, Stream (Of a) m r))
--
-- This function accumulates any effects in the stream
-- until we reach either Return or Step. In the case of
-- Return, we produce a Left, and in the case of Step,
-- we produce a Right with the two components of Of.
--
-- The functions yield and next together with the monadic
-- interface of Stream (including lift), it is possible to
-- define all the subsequent functions without going back
-- to the primitive constructors.

-- S3. Implement
--
-- each :: Monad m => [a] -> Stream (Of a) m ()

-- S4. Implement
--
-- map :: Monad m => (a -> b) -> Stream (Of a) m r -> Stream (Of b) m r
--
-- (You may have to hide the Prelude version of map.)

-- S5. Implement
--
-- stdoutLn :: MonadIO m => Stream (Of String) m () -> m ()
--
-- that prints each string of the incoming stream as soon
-- as it arrives.

-- S6. Verify that
--
-- stdoutLn (map show (each [1..10]))
--
-- works as expected. Does this also work for an infinite list?

-- S7. Implement
--
-- delay :: Int -> Stream (Of ()) m r
--
-- that produces a () with a delay of the given
-- number of milliseconds in between (use threadDelay
-- from Control.Concurrent).

-- S8. Implement
--
-- zipWith :: (a -> b -> c) -> Stream (Of a) m r -> Stream (Of b) m r -> Stream (Of c) m r
--
-- such that the resulting stream produces one result whenever the
-- two incoming streams produce a result.

-- S9. What does
--
-- stdoutLn (zipWith const (each [1..10]) (delay 1000000))
--
-- do? Does this also work for an infinite list?

-- S10. Define
--
-- stdinLn :: MonadIO m => Stream (Of String) m ()
--
-- that consumes stdin line by line.

-- S12. Define
--
-- number :: IO ()
--
-- that reads stdin line by line, and outputs it line
-- by line, prefix each line with its line number,
-- starting from 1.

-- S13. Define
--
-- sum :: (Num a, Monad m) => Stream (Of a) m () -> m a
--
-- that sums up all elements in the stream (in constant
-- space).

-- S14. Define
--
-- catMaybes :: Monad m => Stream (Of (Maybe a)) m r -> Stream (Of a) m r
--
-- that drops all the Nothing occurrences from the incoming
-- stream.

-- S15. Define
--
-- readLn :: (Read a, MonadIO m) => Stream (Of a) m ()
--
-- that reads stdin line by line and parses each line, ignoring
-- failed parses (use readMaybe from Text.Read).

-- S16. Define
--
-- sumAll :: IO ()
--
-- that reads stdin line by line and computes the sum of
-- all the lines that parse as a number.

-- S17. Define
--
-- breaks1 :: Monad m => (a -> Bool) -> Stream (Of a) m r -> Stream (Of [a]) m r
-- breaks2 :: Monad m => (a -> Bool) -> Stream (Of a) m r -> Stream (Of (Stream (Of a) m ())) m r
-- breaks3 :: Monad m => (a -> Bool) -> Stream (Of a) m r -> Stream (Stream (Of a) m) m r
--
-- that all do the same thing: split up the incoming stream at each
-- occurrence of an item matching the given predicate.
--
-- The difference is in the way the chunks are returned. break1 returns
-- each chunk as a list, break2 and break3 return each chunk as a stream,
-- but break3 omits the use of Of.
--
-- Which of the three versions is the best?


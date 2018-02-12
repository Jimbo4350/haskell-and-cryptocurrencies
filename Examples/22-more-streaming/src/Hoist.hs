{-# LANGUAGE RankNTypes #-}

module Hoist where

import           Control.Monad.IO.Class (MonadIO (..))
import           Prelude                hiding (readFile)
import           Stream                 (Stream, StreamF (..), Free (..))
import qualified Stream                 as S
import           System.IO              (Handle, hIsEOF, hGetLine)

hoist :: Functor m => (forall c. m c -> n c) -> Stream b m a -> Stream b n a
hoist _ (Return a)         = Return a 
hoist f (Wrap (Yield b k)) = Wrap $ Yield b $ hoist f k
hoist f (Wrap (Lift m))    = Wrap $ Lift $ f $ fmap (hoist f) m

fromHandleIO :: Handle -> Stream String IO ()
fromHandleIO h = do
    eof <- S.lift $ hIsEOF h
    if eof
        then return ()
        else do
            l <- S.lift $ hGetLine h
            S.yield l
            fromHandleIO h

fromHandleM :: MonadIO m => Handle -> Stream String m ()
fromHandleM = hoist liftIO . fromHandleIO

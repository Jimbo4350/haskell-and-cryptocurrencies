{-# LANGUAGE DeriveFunctor #-}

module Stream where

import Control.Monad          (ap, liftM)
import Control.Monad.IO.Class (MonadIO (..))
import Data.List              hiding (map, take)
import Prelude                hiding (mapM, map, take)

data StreamF b m r =
     Lift (m r)
  |  Yield b r
  deriving (Functor)

data Free f a =
     Return a
  |  Wrap (f (Free f a))

type Stream b m = Free (StreamF b m)

instance Functor f => Monad (Free f) where
  return = Return

  Return x  >>=  f  =  f x
  Wrap c    >>=  f  =  Wrap (fmap (>>= f) c)

instance Functor f => Applicative (Free f) where
   pure = return
   (<*>) = ap

instance Functor f => Functor (Free f) where
   fmap = liftM

yield :: b -> Stream b m ()
yield b = Wrap (Yield b (Return ()))

lift :: Functor m => m a -> Stream b m a
lift m = Wrap (Lift (fmap Return m))

each :: Monad m => [b] -> Stream b m ()
each = foldr ((>>) . yield) (return ())

map :: Monad m => (b -> c) -> Stream b m a -> Stream c m a
map _ (Return x)         = return x
map f (Wrap (Lift m))    = Wrap (Lift $ fmap (map f) m)
map f (Wrap (Yield b k)) = Wrap (Yield (f b) $ map f k)

take :: Monad m => Int -> Stream b m a -> Stream b m ()
take n s
    | n <= 0    = return ()
    | otherwise = case s of
        (Return _)         -> return ()
        (Wrap (Lift m))    -> Wrap (Lift $ fmap (take n) m)
        (Wrap (Yield b k)) -> do
            yield b
            take (n - 1) k

toList :: Monad m => Stream b m () -> m [b]
toList (Return ())        = return []
toList (Wrap (Lift m))    = do
    s <- m
    toList s
toList (Wrap (Yield b k)) = do
    bs <- toList k
    return (b : bs)

mapM :: Monad m =>
  (b -> m c) -> Stream b m a -> Stream c m a
mapM _ (Return x)          =  return x
mapM f (Wrap (Lift m))     =
  Wrap (Lift (fmap (mapM f) m))
mapM f (Wrap (Yield b k))  =  do
  c <- lift (f b)
  yield c
  mapM f k

for :: Monad m =>
  Stream b m a -> (b -> Stream c m r)
    -> Stream c m a
for (Return a)          _ = return a
for (Wrap (Lift m))     f =
  Wrap (Lift (fmap (`for` f) m))
for (Wrap (Yield b k))  f = do
  _ <- f b
  for k f

effects :: Monad m => Stream b m a -> m a
effects (Return x)          =  return x
effects (Wrap (Lift m))     =  m >>= effects
effects (Wrap (Yield _ k))  =  effects k

stdoutLn :: MonadIO m => Stream String m a -> m a
stdoutLn = effects . mapM (liftIO . putStrLn)

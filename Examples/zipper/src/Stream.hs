{-# LANGUAGE DeriveFunctor #-}

module Stream where

import Control.Comonad

data Stream a = Stream a (Stream a)
    deriving Functor

shead :: Stream a -> a
shead (Stream a _) = a

stail :: Stream a -> Stream a
stail (Stream _ s) = s

nat :: Stream Int
nat = go 0
  where go n = Stream n $ go $ n + 1

toList :: Stream a -> [a]
toList (Stream a s) = a : toList s

instance Comonad Stream where

    extract = shead
    duplicate s@(Stream _ t) = Stream s $ duplicate t

example :: Stream Int -> Stream Int
example = extend f
  where
    f :: Stream Int -> Int
    f s =
        let x = shead s
            y = shead $ stail s
        in  x + y

average :: Int -> Stream Double -> Stream Double
average n = extend f
  where
    f :: Stream Double -> Double
    f s = sum (take n $ toList s) / fromIntegral n

wave :: Stream Double
wave = go 0
  where
    go x = Stream (sin x) $ go (x + 0.1)

-- Monad m                                Comonad w
--
-- return :: a -> m a                     extract   :: w a -> a
-- (>>=)  :: m a -> (a -> m b) -> m b     extend    :: (w b -> a) -> w b -> w a
--           (a -> m b) -> (m a -> m b)
-- join   :: m (m a) -> m a               duplicate :: w a -> w (w a)

-- (>>=) ma c = join $ fmap c ma
-- join mma = mma >>= id

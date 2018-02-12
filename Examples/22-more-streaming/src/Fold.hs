{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Fold where

import           Control.Applicative (liftA2)
import qualified Control.Monad       as M
import           Data.Foldable       (foldl')
import           Stream              (Stream, StreamF (..), Free (..))
import qualified Stream              as S

data Fold :: * -> * -> * where
    Fold :: (x -> a -> x) -> x -> (x -> b) -> Fold a b

fold :: Foldable f => Fold a b -> f a -> b
fold (Fold op acc ex) = ex . foldl' op acc

instance Functor (Fold a) where
    fmap f (Fold op acc ex) = Fold op acc (f . ex)

instance Applicative (Fold a) where
    pure x = Fold (\_ _ -> ()) () (const x)

    Fold opf accf exf <*> Fold opx accx exx = Fold op (accf, accx) ex
      where
        op (!xf, !xx) a = (opf xf a, opx xx a)
        ex (!xf, !xx) = exf xf $ exx xx

sum_ :: Num a => Fold a a
sum_ = Fold (+) 0 id

length_ :: Fold a Int
length_ = Fold (const . (+ 1)) 0 id

instance Num b => Num (Fold a b) where
    fromInteger = pure . fromInteger
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    (+) = liftA2 (+)
    (*) = liftA2 (*)

instance Fractional b => Fractional (Fold a b) where
    fromRational = pure . fromRational
    recip = fmap recip

average_ :: Fractional a => Fold a a
average_ = sum_ / (fromIntegral <$> length_)

data FoldM :: (* -> *) -> * -> * -> * where
    FoldM :: (x -> a -> m x) -> x -> (x -> m b) -> FoldM m a b

foldM :: (Monad m, Foldable f) => FoldM m a b -> f a -> m b
foldM (FoldM op acc ex) xs = M.foldM op acc xs >>= ex

instance Functor m => Functor (FoldM m a) where
    fmap f (FoldM op acc ex) = FoldM op acc (fmap f . ex)

instance Applicative m => Applicative (FoldM m a) where
    pure x = FoldM (\_ _ -> pure ()) () (const $ pure x)

    FoldM opf accf exf <*> FoldM opx accx exx = FoldM op (accf, accx) ex
      where
        op (!xf, !xx) a = (,) <$> opf xf a <*> opx xx a
        ex (!xf, !xx) = ($) <$> exf xf <*> exx xx

instance (Applicative m, Num b) => Num (FoldM m a b) where
    fromInteger = pure . fromInteger
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    (+) = liftA2 (+)
    (*) = liftA2 (*)

instance (Applicative m, Fractional b) => Fractional (FoldM m a b) where
    fromRational = pure . fromRational
    recip = fmap recip

foldS :: Monad m => (x -> b -> x) -> x -> (x -> c) -> Stream b m a -> m c
foldS op acc ex = go acc
  where
    go !acc' (Return _)         = return $ ex acc'
    go !acc'(Wrap (Lift m))    = m >>= go acc'
    go !acc'(Wrap (Yield b k)) = go (op acc' b) k

purely :: (forall x. (x -> a -> x) -> x -> (x -> b) -> r) -> Fold a b -> r
purely f (Fold op acc ex) = f op acc ex

test :: IO Double
test = purely foldS average_ $ S.mapM (\x -> putStr "." >> return x) $ S.each [1 .. 10]

{-# LANGUAGE KindSignatures #-}

module Wrapped where

data WrappedInt (f :: * -> *) = Wrap (f Int)

example1 :: WrappedInt Maybe
example1 = Wrap (Just 42)

example2 :: WrappedInt []
example2 = Wrap [1,2,3]

example3 :: WrappedInt IO
example3 = Wrap (read <$> getLine)

class MyFunctor (f :: * -> *) where

    myfmap :: (a -> b) -> f a -> f b

instance MyFunctor ((,,) e f) where

    myfmap :: (a -> b) -> (e, f, a) -> (e, f, b)
    myfmap g (e,f,a) = (e,f, g a)

type Flip f a b = f b a

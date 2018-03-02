{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

{-|
Module      : Codensity
Description : performance of free monads
Copyright   : (c) Lars Bruenjes, 2017
License     : BSD3
Maintainer  : lars.bruenjes@iohk.io
Stability   : experimental
Portability : portable

Sample solution for W6.4.
-}

module Codensity where

import Control.Monad (liftM, ap)

data GP a =
      End a
    | Get (Int -> GP a)
    | Put Int (GP a)

instance Functor GP where
    fmap = liftM

instance Applicative GP where
    pure = return
    (<*>) = ap

instance Monad GP where
    return = End

    End x   >>= f = f x
    Get k   >>= f = Get ((>>= f) . k)
    Put x k >>= f = Put x (k >>= f)

get :: GP Int
get = Get return

put :: Int -> GP ()
put x = Put x (End ())

simulate :: GP a -> [Int] -> a
simulate (End a)   _        = a
simulate (Put _ k) is       = simulate k is
simulate (Get k)   (i : is) = simulate (k i) is
simulate (Get _)   []       = error "input stream exhausted"

askMany :: Int -> GP Int
askMany 0 = return 0
askMany n = do
    x <- askMany (n - 1)
    i <- get
    return (x + i)

newtype GP' a = GP' {unGP' :: forall b. (a -> GP b) -> GP b}

fromGP :: GP a -> GP' a
fromGP m = GP' (m >>=)

toGP :: GP' a -> GP a
toGP m = unGP' m return

get' :: GP' Int
get' = fromGP get

put' :: Int -> GP' ()
put' x = fromGP (put x)

simulate' :: GP' a -> [Int] -> a
simulate' = simulate . toGP

instance Functor GP' where
    fmap = liftM

instance Applicative GP' where
    pure = return
    (<*>) = ap

instance Monad GP' where
    return a = GP' $ \k -> k a

    GP' m >>= f = GP' $ m . flip (unGP' . f)

    -- m                      :: forall c. (a -> GP c) -> GP c
    -- k                      :: b -> GP c
    -- f                      :: a -> GP' b
    -- unGP' . f              :: a -> (forall c. (b -> GP c) -> GP c)
    --                         = forall c. a -> (b -> GP c) -> GP c
    -- flip (unGP' . f)       :: forall c. (b -> GP c) -> a -> GP c
    -- flip (unGP' . f) k     :: a -> GP c
    -- m $ flip (unGP' . f) k :: GP c
    -- m . flip (unGP' . f)   :: forall c. (b -> GP c) -> GP c

askMany' :: Int -> GP' Int
askMany' 0 = return 0
askMany' n = do
    x <- askMany' (n - 1)
    i <- get'
    return (x + i)

{-# OPTIONS_HADDOCK ignore-exports #-}

{-|
Module      : Stack.Core
Description : core definitions for the Stack language
Copyright   : (c) Lars Bruenjes, 2017
License     : BSD3
Maintainer  : lars.bruenjes@iohk.io
Stability   : experimental
Portability : portable

This module contains the core definitions for the Stack language
and functions to execute Stack programs.
-}

module Stack.Core
    ( Instructions (..)
    , run
    , run'
    ) where

import Control.Monad

-- |Definition of the (abstract) syntax of the Stack language.
data Instructions =
    Push Int Instructions
  | Add Instructions
  | Mul Instructions
  | Dup Instructions
  | Swap Instructions
  | Neg Instructions
  | Pop Instructions
  | Over Instructions
  | IfZero Instructions Instructions
  | Loop (Instructions -> Instructions)
  | Halt

newtype MState a = MState {runMState :: [Int] -> Maybe (a, [Int])}

instance Functor MState where

    fmap = liftM

instance Applicative MState where

    pure = return

    (<*>) = ap

instance Monad MState where

    return a = MState $ \xs -> Just (a, xs)

    m >>= cont = MState $ \xs -> case runMState m xs of
        Nothing       -> Nothing
        Just (a, xs') -> runMState (cont a) xs'

get :: MState [Int]
get = MState $ \xs -> Just (xs, xs)

put :: [Int] -> MState ()
put xs = MState $ const $ Just ((), xs)

modify :: ([Int] -> [Int]) -> MState ()
modify f = get >>= (put . f)

pop :: MState Int
pop = do
    xs <- get
    case xs of
        []        -> throw
        (x : xs') -> put xs' >> return x

push :: Int -> MState ()
push n = modify (n :)

peek :: MState Int
peek = do
    n <- pop
    push n
    return n

throw :: MState a
throw = MState $ const Nothing

run :: Instructions -> Maybe [Int]
run ins = run' ins []

run' :: Instructions -> [Int] -> Maybe [Int]
run' ins xs = snd <$> runMState (runMS ins) xs

runMS :: Instructions -> MState ()
runMS (Push n ins)     = push n >> runMS ins
runMS (Add ins)        = pop >>= \m -> pop >>= \n -> push (m + n) >> runMS ins
runMS (Mul ins)        = pop >>= \m -> pop >>= \n -> push (m * n) >> runMS ins
runMS (Dup ins)        = peek >>= push >> runMS ins
runMS (Swap ins)       = pop >>= \m -> pop >>= \n -> push m >> push n >> runMS ins
runMS (Neg ins)        = pop >>= \n -> push (-n) >> runMS ins
runMS (Pop ins)        = pop >> runMS ins
runMS (Over ins)       = pop >>= \m -> peek >>= \n -> push m >> push n >> runMS ins
runMS (IfZero thn els) = pop >>= \n -> runMS $ if n == 0 then thn else els
runMS l@(Loop f)       = runMS $ f l
runMS Halt             = return ()

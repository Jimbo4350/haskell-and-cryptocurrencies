module State where

import Control.Monad (ap, liftM)

-- f $ s = f s

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
    fmap = liftM

instance Applicative (State s) where
    pure  = return
    (<*>) = ap

instance Monad (State s) where

    return :: a -> State s a
    return a = State $ \s -> (a, s)

    (>>=) :: State s a -> (a -> State s b) -> State s b
    (>>=) (State f) k = State $ \s -> let (a, s') = f s
                                          State g = k a
                                      in  g s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

example :: State Int ()
example = do
    s <- get
    put $ s + 42

example' :: State Int ()
example' = get >>= put . (+ 42)

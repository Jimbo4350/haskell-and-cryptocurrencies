{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

module Life.Tape
    ( Tape (..)
    , left
    , right
    , center
    , shift
    , at
    ) where

import           Control.Comonad (Comonad (..))
import           Control.Lens    (Lens', lens, (^.), (.~), (&))
import           Data.Stream     (Stream)
import qualified Data.Stream     as S

data Tape a = Tape (Stream a) a (Stream a) deriving Functor

instance Applicative Tape where

    pure x = Tape (pure x) x (pure x)

    (Tape fs g hs) <*> (Tape xs y zs) = Tape (fs <*> xs) (g y) (hs <*> zs)

showTape :: Show a => Int -> Tape a -> String
showTape n (Tape xs y zs) =
    let xs' = show <$> reverse (S.take n xs)
        zs' = show <$> S.take n zs
    in  "... " ++ unwords xs' ++ "   " ++ show y ++ "   " ++ unwords zs' ++ " ..."

instance Show a => Show (Tape a) where

    show = showTape 3

left :: Tape a -> Tape a
left (Tape xs y zs) = Tape (S.tail xs) (S.head xs) (S.Cons y zs)

right :: Tape a -> Tape a
right (Tape xs y zs) = Tape (S.Cons y xs) (S.head zs) (S.tail zs)

center :: Lens' (Tape a) a
center = lens
    (\(Tape _ y _) -> y)
    (\(Tape xs _ zs) y -> Tape xs y zs)

instance Comonad Tape where

    extract = (^. center)

    duplicate t = Tape (S.tail $ S.iterate left t) t (S.tail $ S.iterate right t)

shift :: Int -> Tape a -> Tape a
shift n t
    | n == 0    = t
    | n < 0     = shift (succ n) $ left t
    | otherwise = shift (pred n) $ right t

at :: Int -> Lens' (Tape a) a
at n
    | n == 0    = center
    | otherwise = lens
        (extract . shift n)
        (\t y -> shift (negate n) $ shift n t & center .~ y)

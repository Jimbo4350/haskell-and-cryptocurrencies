{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

module Life.Grid
    ( Grid
    , at
    , center
    , shift
    , left
    , right
    , up
    , down
    ) where

import           Control.Comonad (Comonad (..))
import           Control.Lens    (Lens', iso, (^.))
import qualified Data.List       as L
import qualified Data.Stream     as S
import           Life.Tape       (Tape)
import qualified Life.Tape       as T

newtype Grid a = Grid {unGrid :: Tape (Tape a)} deriving Functor

instance Applicative Grid where

    pure = Grid . pure . pure

    (Grid f) <*> (Grid x) = Grid ((<*>) <$> f <*> x)

at :: Int -> Int -> Lens' (Grid a) a
at dx dy = iso unGrid Grid . T.at dx . T.at dy

center :: Lens' (Grid a) a
center = at 0 0

showGrid :: Show a => Int -> Grid a -> String
showGrid n g = L.intercalate "\n" [row dy | dy <- [(-n) .. n]]

  where

    row :: Int -> String
    row dy = unwords $ map show [g ^. at dx dy | dx <- [(-n) .. n]]

instance Show a => Show (Grid a) where

    show = showGrid 3

shift :: Int -> Int -> Grid a -> Grid a
shift dx dy = Grid . fmap (T.shift dy) . T.shift dx . unGrid

left, right, up, down :: Grid a -> Grid a
left = shift (-1) 0
right = shift 1 0
up = shift 0 (-1)
down = shift 0 1

instance Comonad Grid where

    extract = (^. center)

    duplicate g =
        let c = T.Tape (S.tail $ S.iterate up g) g (S.tail $ S.iterate down g)
        in  Grid $ T.Tape (S.tail $ S.iterate (fmap left) c) c (S.tail $ S.iterate (fmap right) c)

module Life
    ( Board
    , board
    , step
    , game
    , at
    ) where

import Control.Comonad (Comonad (..))
import Control.Lens    ((&), (^.), (.~))
import Data.List       (foldl')
import Life.Grid

neighbors :: Grid a -> [a]
neighbors g = [g ^. at dx dy | dx <- [(-1) .. 1], dy <- [(-1) .. 1], (dx, dy) /= (0, 0)]

type Board = Grid Bool

board :: [(Int, Int)] -> Board
board = foldl' f $ pure False

  where

    f :: Board -> (Int, Int) -> Board
    f b (dx, dy) = b & at dx dy .~ True

step :: Board -> Board
step = extend f

  where

    f :: Board -> Bool
    f g = let b = g ^. center
              n = length $ filter id $ neighbors g
          in  n == 3 || (b && n == 2)

game :: [(Int, Int)] -> [Board]
game = iterate step . board

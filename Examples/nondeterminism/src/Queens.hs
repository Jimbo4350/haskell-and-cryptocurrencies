module Queens where

import           Control.Monad
import           Control.Monad.State

--guard :: Alternative f => Bool -> f ()

pythagoras :: [(Int, Int, Int)]
pythagoras = do
    c <- [1..]
    b <- [1..c]
    a <- [1..b]
    guard $ a * a + b * b == c * c
    return (a, b, c)

-- . Q . .   . . Q .
-- . . . Q   Q . . .
-- Q . . .   . . . Q
-- . . Q .   . Q . .

-- . . . . . . . Q
-- . Q . . . . . .
-- . . . Q . . . .
-- Q . . . . . . .
-- . . . . . . Q .
-- . . . . Q . . .
-- . . Q . . . . .
-- . . . . . Q . .

queens :: Int -> [[Int]]
queens n = runM (queens' n n) [1..n]

queens' :: Int -> Int -> M [Int]
queens' _ 0 = return []
queens' n k = do
    qs <- queens' n (k - 1)
    q  <- pick
    guard $ f q qs
    return (q : qs)
  where
    f :: Int -> [Int] -> Bool
    f q qs = all (\(i, q') -> q' /= q + i &&
                              q' /= q - i ) $ zip [1..] qs

--   S E N D
-- + M O R E
-- ---------
-- M O N E Y

--   9 5 6 7
-- + 1 0 8 5
-- ---------
-- 1 0 6 5 2

sendMoreMoney :: [(Int, Int, Int)]
sendMoreMoney = runM sendMoreMoneyM [0..9]

sendMoreMoneyM :: M (Int, Int, Int)
sendMoreMoneyM = do
    d <- pick
    e <- pick
    y <- pick
    guard $ mod (d + e) 10 == y
    n <- pick
    r <- pick
    let nd = 10 * n + d
        re = 10 * r + e
        ey = 10 * e + y
    guard $ mod (nd + re) 100 == ey
    o <- pick
    let end = 100 * e + nd
        ore = 100 * o + re
        ney = 100 * n + ey
    guard $ mod (end + ore) 1000 == ney
    s <- pick
    guard $ s /= 0
    m <- pick
    guard $ m /= 0
    let  send  =             s * 1000 + end
         more  =             m * 1000 + ore
         money = m * 10000 + o * 1000 + ney
    guard $ send + more == money
    return (send, more, money)




type M a = StateT [Int] [] a

pick :: M Int
pick = do
    xs      <- get
    (y, ys) <- lift $ g xs
    put ys
    return y

runM :: M a -> [Int] -> [a]
runM = evalStateT

g :: [Int] -> [(Int, [Int])]
g []       = []
g (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- g xs]

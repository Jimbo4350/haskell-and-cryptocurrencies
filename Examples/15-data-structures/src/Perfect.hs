module Perfect where

data Perfect a = Zero a | Suc (Perfect (a, a))
    deriving Show

sumPerfect :: Perfect Int -> Int
sumPerfect = sumPerfect' id

sumPerfect' :: (a -> Int) -> Perfect a -> Int
sumPerfect' f (Zero a) = f a
sumPerfect' f (Suc p)  = sumPerfect' f' p
    where
        f' (a, b) = f a + f b

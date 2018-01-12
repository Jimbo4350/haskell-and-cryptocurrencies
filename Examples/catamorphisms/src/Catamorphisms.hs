{-# LANGUAGE RankNTypes #-}
module Catamorphisms where

type MyMaybe a = forall r. r -> (a -> r) -> r

myMaybeToMaybe :: MyMaybe a -> Maybe a
myMaybeToMaybe f = f Nothing Just

maybeToMyMaybe :: Maybe a -> MyMaybe a
maybeToMyMaybe Nothing  n _ = n
maybeToMyMaybe (Just a) _ j = j a

type MyList a = forall r. (a -> r -> r) -> r -> r

myListToList :: MyList a -> [a]
myListToList f = f (:) []

listToMyList :: [a] -> MyList a
--listToMyList xs c n = foldr c n xs
listToMyList []       _ n = n
listToMyList (x : xs) c n = c x (listToMyList xs c n)

type MyBool = forall r. r -> r -> r

myBoolToBool :: MyBool -> Bool
myBoolToBool f = f True False

boolToMyBool :: Bool -> MyBool
--boolToMyBool b t e = if b then t else e
boolToMyBool True  t _ = t
boolToMyBool False _ e = e

{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}

module List where

import Control.Monad.State
import Text.Read           hiding (get)

data ListZipper a = ListZipper [a] [a]
    deriving Show

fromList :: [a] -> ListZipper a
fromList = ListZipper []

left :: ListZipper a -> Maybe (ListZipper a)
left (ListZipper []       _) = Nothing
left (ListZipper (a : xs) l) = Just $ ListZipper xs (a : l)

toList :: ListZipper a -> [a]
toList z@(ListZipper _ l) = case left z of
    Nothing -> l
    Just z' -> toList z'

right :: ListZipper a -> Maybe (ListZipper a)
right (ListZipper _  [])       = Nothing
right (ListZipper xs (a : ys)) = Just $ ListZipper (a : xs) ys

change :: ([a] -> [a]) -> ListZipper a -> ListZipper a
change f (ListZipper xs l) = ListZipper xs $ f l

inspect :: ListZipper a -> [a]
inspect (ListZipper _ l) = l

set :: [a] -> ListZipper a -> ListZipper a
set t = change $ const t

data Command a = L | R | C a | D | S [a] | Q
    deriving (Show, Read)

apply :: Command a -> ListZipper a -> Either (Maybe (ListZipper a)) [a]
apply L     = Left . left
apply R     = Left . right
apply Q     = Right . toList
apply (C a) = Left . Just . change f
  where
    f []       = [a]
    f (_ : xs) = a : xs
apply D      = Left . Just . change (const [])
apply (S xs) = Left . Just . set xs


editT :: (Show a, Read a) => StateT (ListZipper a) IO [a]
editT = do
    z <- get
    liftIO $ print z
    liftIO $ print $ inspect z
    s <- liftIO getLine
    case readMaybe s of
        Nothing -> liftIO (putStrLn "invalid command") >> editT
        Just c  -> case apply c z of
            Left Nothing   -> liftIO (putStrLn "invalid operation") >> editT
            Left (Just z') -> put z' >> editT
            Right t        -> return t

edit :: (Show a, Read a) => [a] -> IO [a]
edit = evalStateT editT . fromList

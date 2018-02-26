{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}

module Tree where

import Control.Monad.State
import Text.Read           hiding (get)

data Tree a = Tip | Node (Tree a) a (Tree a)
    deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

-- Hole a ~ Either (a, Tree a) (Tree a, a)
data Hole a =   LeftHole a (Tree a)
              | RightHole (Tree a) a
    deriving Show

data TreeZipper a = TreeZipper [Hole a] (Tree a)
    deriving Show

fromTree :: Tree a -> TreeZipper a
fromTree t = TreeZipper [] t

up :: TreeZipper a -> Maybe (TreeZipper a)
up (TreeZipper []                  _)  = Nothing
up (TreeZipper (LeftHole  a r : xs) t) = Just $ TreeZipper xs $ Node t a r
up (TreeZipper (RightHole l a : xs) t) = Just $ TreeZipper xs $ Node l a t

toTree :: TreeZipper a -> Tree a
toTree z@(TreeZipper _ t) = case up z of
    Nothing -> t
    Just z' -> toTree z'

left :: TreeZipper a -> Maybe (TreeZipper a)
left (TreeZipper _  Tip)          = Nothing
left (TreeZipper xs (Node l a r)) = Just $ TreeZipper (LeftHole a r : xs) l

right :: TreeZipper a -> Maybe (TreeZipper a)
right (TreeZipper _  Tip)          = Nothing
right (TreeZipper xs (Node l a r)) = Just $ TreeZipper (RightHole l a : xs) r

change :: (Tree a -> Tree a) -> TreeZipper a -> TreeZipper a
change f (TreeZipper xs t) = TreeZipper xs $ f t

set :: Tree a -> TreeZipper a -> TreeZipper a
set t = change $ const t

data Command a = U | L | R | C a | D | Q
    deriving (Show, Read)

apply :: Command a -> TreeZipper a -> Either (Maybe (TreeZipper a)) (Tree a)
apply U     = Left . up
apply L     = Left . left
apply R     = Left . right
apply Q     = Right . toTree
apply (C a) = Left . Just . change f
  where
    f Tip          = Node Tip a Tip
    f (Node l _ r) = Node l a r
apply D     = Left . Just . change (const Tip)

editT :: (Show a, Read a) => StateT (TreeZipper a) IO (Tree a)
editT = do
    z <- get
    liftIO $ print z
    s <- liftIO getLine
    case readMaybe s of
        Nothing -> liftIO (putStrLn "invalid command") >> editT
        Just c  -> case apply c z of
            Left Nothing   -> liftIO (putStrLn "invalid operation") >> editT
            Left (Just z') -> put z' >> editT
            Right t        -> return t

edit :: (Show a, Read a) => Tree a -> IO (Tree a)
edit = evalStateT editT . fromTree

tree :: Tree String
tree = Node
        (Node Tip "Scala" Tip)
        "is"
        (Node Tip "good" Tip)

-- Tree a = Tip | Node (Tree a) a (Tree a)
-- f (x) = 1 + a * x^2 -- 1 + x * a * x
-- f'(x) = 2ax         -- a * x + x * a
--
-- List a = Nil | Cons a (List a)
-- f (x) = 1 + a * x
-- f'(x) = a

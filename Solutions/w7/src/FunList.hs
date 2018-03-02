module FunList where

import Optics

data FunList a b t =
      Done t
    | More a (FunList a b (b -> t))
    deriving Functor

instance Applicative (FunList a b) where
    pure = Done

    Done g   <*> l = g <$> l
    More a g <*> l = More a $ flip <$> g <*> l
    -- g                :: FunList a b (b -> u -> v)
    -- l                :: FunList a b u
    -- flip <$> g       :: FunList a b (u -> b -> v)
    -- flip <$> g <*> l :: FunList a b (b -> v)

toList :: FunList a b t -> [a]
toList (Done _)   = []
toList (More a l) = a : toList l

fromList :: [a] -> FunList a b [b]
fromList []       = Done []
fromList (a : as) = More a $ flip (:) <$> fromList as

singleton :: a -> FunList a b b
singleton a = More a $ Done id

fuse :: FunList a a t -> t
fuse (Done t)   = t
fuse (More a g) = fuse g a
--g :: FunList a a (a -> t)
--a ::  a

fromFunList' :: Applicative f => (a -> f b) -> FunList a b t -> f t
fromFunList' _ (Done t)   = pure t
fromFunList' g (More a l) = flip ($) <$> g a <*> fromFunList' g l
-- a                                     :: a
-- g a                                   :: f b
-- l                                     :: FunList a b (b -> t)
-- fromFunList' g l                      :: f (b -> t)
-- flip ($) <$> g a <*> fromFunList' g l :: f t
-- (NOTE: fromFunList' g l <*> g a would also have the right type, but would
-- reverse the order of effects!)

toFunList :: Traversal s t a b -> (s -> FunList a b t)
toFunList t = t singleton

fromFunList :: (s -> FunList a b t) -> Traversal s t a b
fromFunList g h = fromFunList' h . g

transform :: (FunList a b t -> FunList a b t) -> (Traversal s t a b -> Traversal s t a b)
transform f t = fromFunList $ (f .) $ toFunList t

taking :: Int -> Traversal' s a -> Traversal' s a
taking m = transform (go m)
  where
    go :: Int -> FunList a a t -> FunList a a t
    go _ l@(Done _) = l
    go 0 l          = Done $ fuse l
    go n (More a f) = More a $ go (n - 1) f

dropping :: Int -> Traversal' s a -> Traversal' s a
dropping m = transform (go m)
  where
    go :: Int -> FunList a a t -> FunList a a t
    go _ l@(Done _) = l
    go 0 l          = l
    go n (More a f) = go (n - 1) $ f <*> pure a

heading :: Traversal' s a -> Traversal' s a
heading = transform go
  where
    go :: FunList a a t -> FunList a a t
    go l@(Done _) = l
    go (More a f) = More a $ Done $ fuse f

tailing :: Traversal' s a -> Traversal' s a
tailing = transform go
  where
    go :: FunList a a t -> FunList a a t
    go l@(Done _) = l
    go (More a f) = f <*> pure a

filtering :: forall a s. (a -> Bool) -> Traversal' s a -> Traversal' s a
filtering p = transform go
  where
    go :: FunList a a t -> FunList a a t
    go l@(Done _) = l
    go (More a f)
        | p a       = More a $ go f
        | otherwise = go f <*> pure a

-- The traversal focussing on nothing.
empty :: Traversal' s a
empty = const pure

element :: Int -> Traversal' s a -> Traversal' s a
element n t
    | n < 0     = empty
    | n == 0    = heading t
    | otherwise = element (n - 1) $ tailing t

module Traversal where

import Data.Functor.Const    (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Monoid           (First (..))

type Traversal s a = forall f. Applicative f => (a -> f a) -> (s -> f s)

over :: ((a -> Identity a) -> s -> Identity s) -> (a -> a) -> s -> s
over sa f s = runIdentity (sa (Identity . f) s)

view :: ((a -> Const a a) -> s -> Const a s) -> s -> a
view sa s = getConst (sa Const s)

toListOf :: ((a -> Const [a] a) -> s -> Const [a] s) -> s -> [a]
toListOf p = getConst . p (Const . return)

preview :: ((a -> Const (First a) a) -> s -> Const (First a) s) -> s -> Maybe a
preview p = getFirst . getConst . p (Const . First . Just)

set :: ((a -> Identity a) -> s -> Identity s) -> s -> a -> s
set sa s a = over sa (const a) s

each :: Traversable t => Traversal (t a) a
each = traverse

both :: Traversal (a, a) a
both f (a, b) = (,) <$> f a <*> f b

ignored :: Traversal s a
ignored = const pure

module Polymorphic.Traversal where

import Data.Functor.Const    (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Monoid           (First (..))

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> (s -> f t)

type Traversal' s a = Traversal s s a a

over :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
over sa f s = runIdentity (sa (Identity . f) s)

view :: ((a -> Const a b) -> s -> Const a t) -> s -> a
view sa s = getConst (sa Const s)

toListOf :: ((a -> Const [a] a) -> s -> Const [a] s) -> s -> [a]
toListOf p = getConst . p (Const . return)

preview :: ((a -> Const (First a) a) -> s -> Const (First a) s) -> s -> Maybe a
preview p = getFirst . getConst . p (Const . First . Just)

set :: ((a -> Identity b) -> s -> Identity t) -> s -> b -> t
set sa s a = over sa (const a) s

each :: Traversable t => Traversal (t a) (t b) a b
each = traverse

both :: Traversal (a, a) (b, b) a b
both f (a, a') = (,) <$> f a <*> f a'

ignored :: Traversal' s a
ignored = const pure

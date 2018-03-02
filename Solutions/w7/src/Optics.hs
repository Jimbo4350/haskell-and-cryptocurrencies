module Optics where

import Data.Functor.Const     (Const (..))
import Data.Functor.Identity  (Identity (..))
import Data.Monoid            (First (..))
import Data.Profunctor        (Profunctor (..))
import Data.Profunctor.Choice (Choice (..))
import Data.Tagged            (Tagged (..))

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

type Traversal' s a = Traversal s s a a

type Prism s t a b = forall f p. (Applicative f, Choice p) => p a (f b) -> p s (f t)

type Prism' s a = Prism s s a a

type Iso s t a b = forall f p. (Functor f, Choice p) => p a (f b) -> p s (f t)

type Iso' s a = Iso s s a a

each :: Traversable t => Traversal (t a) (t b) a b
each = traverse

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens gt st f s = st s <$> f (gt s)

view :: ((a -> Const a b) -> s -> Const a t) -> s -> a
view f = getConst . f Const

over :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
over f g = runIdentity . f (Identity . g)

set :: ((a -> Identity b) -> s -> Identity t) -> s -> b -> t
set f s b = over f (const b) s

prism :: (s -> Either t a) -> (b -> t) -> Prism s t a b
prism pr rv p = dimap pr (either pure (fmap rv)) (right' p)

prism' :: (s -> Maybe a) -> (a -> s) -> Prism' s a
prism' pr = prism pr'
  where
    pr' s = case pr s of
        Nothing -> Left s
        Just a  -> Right a

preview :: ((a -> Const (First a) b) -> s -> Const (First a) t) -> s -> Maybe a
preview f = getFirst . getConst . f (Const . First . Just)

review :: (Tagged a (Identity b) -> Tagged s (Identity t)) -> b -> t
review f b = runIdentity $ unTagged $ f (Tagged (Identity b))

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso v r = dimap v (fmap r)

re :: Iso s t a b -> Iso b a t s
re i = iso (review i) (view i)

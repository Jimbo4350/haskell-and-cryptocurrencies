module Iso where

import Data.Profunctor        (Profunctor (..))
import Data.Profunctor.Choice (Choice (..))
import Prism
import Traversal

type Iso s a = forall f p. (Functor f, Choice p) => p a (f a) -> p s (f s)

iso :: (s -> a) -> (a -> s) -> Iso s a
iso v r = dimap v (fmap r)

re :: Iso s a -> Iso a s
re i = iso (review i) (view i)

reversed :: Iso [a] [a]
reversed = undefined

curried :: Iso ((a, b) -> c) (a -> b -> c)
curried = undefined

flipped :: Iso (a -> b -> c) (b -> a -> c)
flipped = undefined

swapped :: Iso (a, b) (b, a)
swapped = undefined

swapped' :: Iso (Either a b) (Either b a)
swapped' = undefined

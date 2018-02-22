module Polymorphic.Iso where

import qualified Data.ByteString        as S
import qualified Data.ByteString.Lazy   as L
import           Data.Profunctor        (Profunctor (..))
import           Data.Profunctor.Choice (Choice (..))
import           Polymorphic.Prism
import           Polymorphic.Traversal

type Iso s t a b = forall p f. (Choice p, Functor f) => p a (f b) -> p s (f t)

type Iso' s a = Iso s s a a

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa as = dimap sa (fmap as)

re :: Iso s t a b -> Iso b a t s
re sa = iso (review sa) (view sa)

lazy :: Iso' S.ByteString L.ByteString
lazy = iso L.fromStrict L.toStrict

reversed :: Iso [a] [b] [a] [b]
reversed = iso reverse reverse

curried :: Iso ((a, b) -> c) ((a', b') -> c') (a -> b -> c) (a' -> b' -> c')
curried = iso curry uncurry

flipped :: Iso (a -> b -> c) (a' -> b' -> c') (b -> a -> c) (b' -> a' -> c')
flipped = iso flip flip

swapped :: Iso (a, b) (a', b') (b, a) (b', a')
swapped = iso swap swap
  where
    swap (a, b) = (b, a)

swapped' :: Iso (Either a b) (Either a' b') (Either b a) (Either b' a')
swapped' = iso swap swap
  where
    swap = either Right Left

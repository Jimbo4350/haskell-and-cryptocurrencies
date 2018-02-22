module Monomorphic.Iso where

import qualified Data.ByteString        as S
import qualified Data.ByteString.Lazy   as L
import           Data.Profunctor        (Profunctor (..))
import           Data.Profunctor.Choice (Choice (..))
import           Monomorphic.Prism
import           Monomorphic.Traversal

type Iso s a = forall p f. (Choice p, Functor f) => p a (f a) -> p s (f s)

iso :: (s -> a) -> (a -> s) -> Iso s a
iso sa as = dimap sa (fmap as)

re :: Iso s a -> Iso a s
re sa = iso (review sa) (view sa)

lazy :: Iso S.ByteString L.ByteString
lazy = iso L.fromStrict L.toStrict

reversed :: Iso [a] [a]
reversed = iso reverse reverse

curried :: Iso ((a, b) -> c) (a -> b -> c)
curried = iso curry uncurry

flipped :: Iso (a -> b -> c) (b -> a -> c)
flipped = iso flip flip

swapped :: Iso (a, b) (b, a)
swapped = iso swap swap
  where
    swap (a, b) = (b, a)

swapped' :: Iso (Either a b) (Either b a)
swapped' = iso swap swap
  where
    swap = either Right Left

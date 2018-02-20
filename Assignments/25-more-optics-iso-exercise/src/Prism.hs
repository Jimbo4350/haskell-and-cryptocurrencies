module Prism where

import Data
import Data.Either            (either)
import Data.Functor.Identity  (Identity (..))
import Data.Profunctor        (Profunctor (..))
import Data.Profunctor.Choice (Choice (..))
import Data.Tagged            (Tagged (..))

type Prism s a = forall f p. (Applicative f, Choice p) => p a (f a) -> p s (f s)

prism :: forall a s. (s -> Maybe a) -> (a -> s) -> Prism s a
prism p r f = dimap g h f'
  where
    f'  = right' f -- p (Either s a) (Either s (f a))
    g s = maybe (Left s) Right (p s)
    h   = either pure (fmap r)

review :: (Tagged a (Identity a) -> Tagged s (Identity s)) -> a -> s
review p = runIdentity . unTagged . p . Tagged . Identity

_Ok :: Prism (Result a) a
_Ok = prism p Ok
  where
    p (Ok a)    = Just a
    p (Error _) = Nothing

_Error :: Prism (Result a) String
_Error = prism p Error
  where
    p (Ok _)    = Nothing
    p (Error e) = Just e

_Left :: Prism (Either a b) a
_Left = prism (either Just (const Nothing)) Left

_Right :: Prism (Either a b) b
_Right = prism (either (const Nothing) Just) Right

_Just :: Prism (Maybe a) a
_Just = prism id Just

_Nothing :: Prism (Maybe a) ()
_Nothing = prism (maybe (Just ()) (const Nothing)) (const Nothing)

_Cons :: Prism [a] (a, [a])
_Cons = prism p r
  where
    p []       = Nothing
    p (x : xs) = Just (x, xs)

    r = uncurry (:)

_Nil :: Prism [a] ()
_Nil = prism p (const [])
  where
    p []      = Just ()
    p (_ : _) = Nothing

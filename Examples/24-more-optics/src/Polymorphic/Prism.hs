module Polymorphic.Prism where

import Data
import Data.Either            (either)
import Data.Functor.Identity  (Identity (..))
import Data.Profunctor        (Profunctor (..))
import Data.Profunctor.Choice (Choice (..))
import Data.Tagged            (Tagged (..))

type Prism s t a b = forall f p. (Applicative f, Choice p) => p a (f b) -> p s (f t)

type Prism' s a = Prism s s a a

prism :: (s -> Either t a) -> (b -> t) -> Prism s t a b
prism pr rv p = dimap pr (either pure (fmap rv)) (right' p)

prism' :: (s -> Maybe a) -> (a -> s) -> Prism' s a
prism' pr = prism pr'
  where
    pr' s = case pr s of
        Nothing -> Left s
        Just a  -> Right a

review :: (Tagged a (Identity b) -> Tagged s (Identity t)) -> b -> t
review p = runIdentity . unTagged . p . Tagged . Identity

_Ok :: Prism' (Result a) a
_Ok = prism' p Ok
  where
    p (Ok a)    = Just a
    p (Error _) = Nothing

_Error :: Prism' (Result a) String
_Error = prism' p Error
  where
    p (Ok _)    = Nothing
    p (Error e) = Just e

_Left :: Prism (Either a b) (Either a' b) a a'
_Left = prism (either Right (Left . Right)) Left

_Right :: Prism (Either a b) (Either a b') b b'
_Right = prism (either (Left . Left) Right) Right

_Just :: Prism (Maybe a) (Maybe b) a b
_Just = prism (maybe (Left Nothing) Right) Just

_Nothing :: Prism' (Maybe a) ()
_Nothing = prism' (maybe (Just ()) (const Nothing)) (const Nothing)

_Cons :: Prism [a] [b] (a, [a]) (b, [b])
_Cons = prism p r
  where
    p []       = Left []
    p (x : xs) = Right (x, xs)

    r = uncurry (:)

_Nil :: Prism' [a] ()
_Nil = prism' p (const [])
  where
    p []      = Just ()
    p (_ : _) = Nothing

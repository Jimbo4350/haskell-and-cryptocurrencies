module NaivePrism where

import Control.Category (Category (..))
import Control.Monad    ((>=>))
import Data
import Prelude          hiding ((.), id)

data Prism s a = Prism
    { preview :: s -> Maybe a
    , review  :: a -> s
    }

prism :: (s -> Maybe a) -> (a -> s) -> Prism s a
prism = Prism

compose :: Prism a x -> Prism s a -> Prism s x
compose ax sa = Prism
    { preview = preview sa >=> preview ax
    , review  = review sa . review ax
    }

instance Category Prism where
    id  = Prism {preview = Just, review = id}
    (.) = compose

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

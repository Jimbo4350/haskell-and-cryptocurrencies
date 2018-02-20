module NaivePrism where

data Prism s a = Prism
    { preview' :: s -> Maybe a
    , review'  :: a -> s
    }

prism :: (s -> Maybe a) -> (a -> s) -> Prism s a
prism = Prism

_Nil :: Prism [a] ()
_Nil = undefined

_Just :: Prism (Maybe a) a
_Just = undefined

_Nothing :: Prism (Maybe a) ()
_Nothing = undefined

_Left :: Prism (Either a b) a
_Left = undefined

_Right :: Prism (Either a b) b
_Right = undefined

_Cons :: Prism [a] (a, [a])
_Cons = undefined

_Id :: Prism a a
_Id = undefined

compose :: Prism a x -> Prism s a -> Prism s x
compose = undefined

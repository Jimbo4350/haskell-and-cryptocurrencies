module Lens where

import Data

type Lens s a = forall f. Functor f => (a -> f a) -> s -> f s

lens :: (s -> a) -> (s -> a -> s) -> Lens s a
lens gt st f s = st s <$> f (gt s)

_1 :: Lens (a, b) a
_1 = lens fst (\(_, b) a -> (a, b))

_2 :: Lens (a, b) b
_2 = lens snd (\(a, _) b -> (a, b))

staff :: Lens Company [Person]
staff = lens _staff (\c ps -> c{_staff = ps})

name :: Lens Person String
name = lens _name (\p n -> p{_name = n})

address :: Lens Person Address
address = lens _address (\p a -> p{_address = a})

city :: Lens Address String
city = lens _city (\a c -> a{_city = c})

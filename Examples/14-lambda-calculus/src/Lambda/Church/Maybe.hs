module Lambda.Church.Maybe
    ( nothing
    , just
    , maybe'
    , isNothing'
    , isJust'
    ) where

import Lambda.Church.Bool
import Lambda.Church.Either
import Lambda.Combinators
import Lambda.Core
import Lambda.Reduction

nothing :: Term
nothing = normalize $ left' :$ true

just :: Term
just = normalize $ Lambda "x" $ right' :$ "x"

maybe' :: Term
maybe' = normalize $ Lambda "m" $ Lambda "n" $ Lambda "f" (either' :$ "m" :$ (k :$ "n") :$ "f")

isNothing' :: Term
isNothing' = normalize $ Lambda "m" $ isLeft' :$ "m"

isJust' :: Term
isJust' = normalize $ Lambda "m" $ isRight' :$ "m"

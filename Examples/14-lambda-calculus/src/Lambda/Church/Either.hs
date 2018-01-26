module Lambda.Church.Either
    ( either'
    , left'
    , right'
    , isLeft'
    , isRight'
    ) where

import Lambda.Church.Bool
import Lambda.Combinators
import Lambda.Core
import Lambda.Reduction

either' :: Term
either' = Lambda "e" $ Lambda "f" $ Lambda "s" $ "e" :$ "f" :$ "s"

left' :: Term
left' = Lambda "x" $ Lambda "f" $ Lambda "s" $ "f" :$ "x"

right' :: Term
right' = Lambda "x" $ Lambda "f" $ Lambda "s" $ "s" :$ "x"

isLeft' :: Term
isLeft' = normalize $ Lambda "e" $ either' :$ "e" :$ (k :$ true) :$ (k :$ false)

isRight' :: Term
isRight' = normalize $ Lambda "e" $ either' :$ "e" :$ (k :$ false) :$ (k :$ true)

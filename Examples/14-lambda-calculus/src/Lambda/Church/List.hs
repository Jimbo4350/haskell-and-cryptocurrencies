module Lambda.Church.List
    ( nil
    , cons
    , isNil
    , head'
    , tail'
    ) where

import Lambda.Church.Bool
import Lambda.Church.Pair
import Lambda.Core
import Lambda.Reduction

nil :: Term
nil = normalize $ pair :$ false :$ false

cons :: Term
cons = normalize $ Lambda "h" $ Lambda "t" $ pair :$ true :$ (pair :$ "h" :$ "t")

isNil :: Term
isNil = normalize $ Lambda "l" $ not' :$ (fst' :$ "l")

head' :: Term
head' = normalize $ Lambda "l" $ fst' :$ (snd' :$ "l")

tail' :: Term
tail' = normalize $ Lambda "l" $ snd' :$ (snd' :$ "l")

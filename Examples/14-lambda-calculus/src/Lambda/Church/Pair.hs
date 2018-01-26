module Lambda.Church.Pair
    ( pair
    , fst'
    , snd'
    ) where

import Lambda.Core

pair :: Term
pair = Lambda "x" $ Lambda "y" $ Lambda "f" $ "f" :$ "x" :$ "y"

fst' :: Term
fst' = Lambda "p" $ "p" :$ Lambda "x" (Lambda "y" "x")

snd' :: Term
snd' = Lambda "p" $ "p" :$ Lambda "x" (Lambda "y" "y")

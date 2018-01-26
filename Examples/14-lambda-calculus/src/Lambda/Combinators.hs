module Lambda.Combinators
    ( i
    , k
    , s
    , y
    , omega
    ) where

import Lambda.Core

i :: Term
i = Lambda "x" "x"

k :: Term
k = Lambda "x" $ Lambda "y" "x"

s :: Term
s = Lambda "x" $ Lambda "y" $ Lambda "z" $ "x" :$ "z" :$ ("y" :$ "z")

y :: Term
y = let t = Lambda "x" $ "f" :$ ("x" :$ "x")
    in  Lambda "f" (t :$ t)

omega :: Term
omega = let t = Lambda "x" ("x" :$ "x") in t :$ t

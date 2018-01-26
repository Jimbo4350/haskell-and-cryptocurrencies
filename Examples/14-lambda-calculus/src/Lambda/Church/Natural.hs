module Lambda.Church.Natural
    ( Natural
    , zero
    , succ'
    , add
    , mul
    , pred'
    , pred''
    , isZero
    , factorial
    , fromNatural
    ) where

import Lambda.Church.Bool
import Lambda.Church.Pair
import Lambda.Combinators
import Lambda.Core
import Lambda.Reduction
import Numeric.Natural    (Natural)

zero :: Term
zero = Lambda "f" $ Lambda "x" "x"

succ' :: Term
succ' = Lambda "n" $ Lambda "f" $ Lambda "x" $ "f" :$ ("n" :$ "f" :$ "x")

add :: Term
add = Lambda "m" $ Lambda "n" $ Lambda "f" $ Lambda "x" $ "m" :$ "f" :$ ("n" :$ "f" :$ "x")

mul :: Term
mul = Lambda "m" $ Lambda "n" $ Lambda "f" $ Lambda "x" $ "m" :$ ("n" :$ "f") :$ "x"

isZero :: Term
isZero = Lambda "n" $ "n" :$ Lambda "x" false :$ true

pred' :: Term
pred' = let f = Lambda "p" $ ifThenElse
                    :$ (fst' :$ "p")
                    :$ (pair :$ false :$ zero)
                    :$ (pair :$ false :$ (succ' :$ (snd' :$ "p")))
            t = pair :$ true :$ zero
            p = Lambda "n" $ snd' :$ ("n" :$ f :$ t)
        in  normalize p

-- |Harder to understand, but much more efficient...
pred'' :: Term
pred'' = Lambda "n" $ Lambda "f" $ Lambda "x" $
    "n" :$ Lambda "g" (Lambda "h" $ "h" :$ ("g" :$ "f")) :$ Lambda "u" "x" :$ Lambda "u" "u"

factorial :: Term
factorial = y :$ f
  where
    f = Lambda "f" $ Lambda "n" $ ifThenElse :$
            (isZero :$ "n") :$
            (succ' :$ zero) :$
            (mul :$ "n" :$ ("f" :$ (pred'' :$ "n")))

fromNatural :: Natural -> Term
fromNatural 0 = zero
fromNatural n = normalize $ succ' :$ fromNatural (n - 1)

module Lambda.Church.Bool
    ( false
    , true
    , ifThenElse
    , not'
    ) where

import Lambda.Core
import Lambda.Reduction

false, true :: Term
false = Lambda "x" $ Lambda "y" "y"
true  = Lambda "x" $ Lambda "y" "x"

ifThenElse :: Term
ifThenElse = Lambda "b" $ Lambda "t" $ Lambda "e" $ "b" :$ "t" :$ "e"

not' :: Term
not' = run cbn $ Lambda "b" $ ifThenElse :$ "b" :$ false :$ true

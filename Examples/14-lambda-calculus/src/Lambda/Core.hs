module Lambda.Core
    ( Term (..)
    , free
    , subst
    , letIn
    ) where

import           Data.Set    (Set)
import qualified Data.Set    as S
import           Data.String (IsString (..))

infixl 1 :$

data Term =
      Var String
    | Lambda String Term
    | (:$) Term Term

instance IsString Term where

    fromString = Var

instance Show Term where

    show (Var x)      = x
    show l@(Lambda _ _) = let (xs, t) = f l in '\x03BB' : unwords xs ++ "." ++ show t
      where
        f :: Term -> ([String], Term) -- return all variable names of successive abstractions and the body
        f (Lambda x t) = let (xs, t') = f t in (x : xs, t')
        f t            = ([], t)
    show (s :$ t)      = f s ++ " " ++ g t
      where
        f l@(Lambda _ _) = p l    -- parenthesize a lambda operator
        f u              = show u -- don't paranthesize variables or applications

        g v@(Var _)  = show v     -- don't parenthesize variables
        g u          = p u        -- parenthesize every other form of operand

        p x = '(' : show x ++ ")" -- put parentheses around the string representation of a term

instance Eq Term where

    Var x          == Var y          = x == y
    Var _          == _              = False
    (s :$ t)       == (s' :$ t')     = s == s' && t == t'
    (_ :$ _)       == _              = False
    k@(Lambda x s) == l@(Lambda y t) =
        let xy = varNameNotIn $ free k `S.union` free l -- to compare two abstractions, first alpha-convert them to have the same variable
            s' = subst x (Var xy) s
            t' = subst y (Var xy) t
        in  s' == t'
    (Lambda _ _)   == _              = False

free :: Term -> Set String
free (Var x)      = S.singleton x
free (Lambda x t) = S.delete x $ free t
free (s :$ t)     = free s `S.union` free t

subst :: String -> Term -> Term -> Term
subst x s = go
  where
    fs :: Set String
    fs = free s

    go t@(Var y)
        | x == y          = s
        | otherwise       = t
    go t@(Lambda y e)
        | x == y          = t
        | y `S.member` fs =
            let y' = varNameNotIn $ fs `S.union` free t
                e' = subst y (Var y') e
            in  go $ Lambda y' e'
        | otherwise       = Lambda y $ go e
    go (t :$ u)           = go t :$ go u

varNameNotIn :: Set String -> String
varNameNotIn s = head $ dropWhile (`S.member` s) ["x" ++ show i | i <- [(0 :: Integer) ..]]

letIn :: String -> Term -> Term -> Term
letIn x s t = Lambda x t :$ s

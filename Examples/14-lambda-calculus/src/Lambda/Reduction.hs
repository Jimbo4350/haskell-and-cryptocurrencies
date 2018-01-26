module Lambda.Reduction
    ( Rule (..)
    , Strategy
    , step
    , run
    , runIO
    , cbn
    , cbv
    , normalize
    , ruleAppLeft
    , ruleAppRight
    , ruleBeta
    , ruleLambda
    ) where

import Control.Monad (msum)
import Lambda.Core

newtype Rule = Rule {runRule :: (Term -> Maybe Term) -> Term -> Maybe Term}
type Strategy = [Rule]

step :: Strategy -> Term -> Maybe Term
step strategy t = msum $ map (\s -> runRule s (step strategy) t) strategy

run :: Strategy -> Term -> Term
run ss = go
  where
    go t = case step ss t of
        Nothing -> t
        Just t' -> go t'

runIO :: Strategy -> Term -> IO Term
runIO ss = go
  where
    go t = do
        print t
        case step ss t of
            Nothing -> return t
            Just t' -> go t'

cbv :: Strategy
cbv = [ ruleLambda
      , ruleAppLeft
      , ruleAppRight
      , ruleBeta
      ]

cbn :: Strategy
cbn = [ ruleLambda
      , ruleBeta
      , ruleAppLeft
      , ruleAppRight
      ]

normalize :: Term -> Term
normalize = run cbn

ruleAppLeft :: Rule
ruleAppLeft = Rule $ \step' term -> case term of
    (s :$ t) -> (:$ t) <$> step' s
    _        -> Nothing

ruleAppRight :: Rule
ruleAppRight = Rule $ \step' term -> case term of
    (s :$ t) -> (s :$) <$> step' t
    _        -> Nothing

ruleBeta :: Rule
ruleBeta = Rule $ \_ term -> case term of
    (Lambda x s :$ t) -> Just $ subst x t s
    _                 -> Nothing

ruleLambda :: Rule
ruleLambda = Rule $ \step' term -> case term of
    (Lambda x s) -> Lambda x <$> step' s
    _            -> Nothing

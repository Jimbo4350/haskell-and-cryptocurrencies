{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Interpreter where

import           Control.Monad.Except
import           Control.Monad.Identity (Identity (..))
import           Control.Monad.State
import           Data.Map               (Map)
import qualified Data.Map               as M

data Expr =
      Lit Int
    | Add Expr Expr
    | Div Expr Expr
    | Var String
    | Seq Expr Expr
    | Assign String Expr
    deriving Show

class Monad m => MonadEval m where

    divisionByZero :: m a
    lookupVar :: String -> m Int
    unknownVar :: String -> m a
    assign :: String -> Int -> m ()
    runEval :: m a -> Env -> Either String a

eval :: MonadEval m => Expr -> m Int
eval (Lit n)        = pure n
eval (Add e f)      = (+) <$> eval e <*> eval f
eval (Div e f)      = do
    y <- eval f
    if y == 0
        then divisionByZero
        else do
            x <- eval e
            return $ x `div` y
eval (Var var)      = lookupVar var
eval (Seq e f)      = eval e >> eval f
eval (Assign var e) = do
    n <- eval e
    assign var n
    return n

newtype Eval a = Eval (StateT Env (ExceptT String Identity) a)
    deriving (Functor, Applicative, Monad, MonadError String, MonadState Env)    

instance MonadEval Eval where

    divisionByZero :: Eval a
    divisionByZero = throwError "division by zero!"

    lookupVar :: String -> Eval Int
    lookupVar var = do
        env <- get
        case M.lookup var env of
            Just n  -> return n
            Nothing -> unknownVar var

    unknownVar :: String -> Eval a
    unknownVar var = throwError $ "unknown var: " ++ var

    assign :: String -> Int -> Eval ()
    assign var n = modify $ M.insert var n

    runEval :: Eval a -> Env -> Either String a
    runEval (Eval m) env = runIdentity $ runExceptT (evalStateT m env)

type Env = Map String Int

{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}

module Free where

import Control.Monad
import qualified Interpreter as MT

{-
data Eval :: * -> * where
    DivByZeroError :: Eval a
    UnknownVar :: String -> Eval a
    VarLookup :: String -> Eval Int
    VarSet :: String -> Int -> Eval ()
    Return :: a -> Eval a
    Bind :: Eval a -> (a -> Eval b) -> Eval b
-}

data EvalOp b =
      DivByZeroError
    | UnknownVar String
    | VarLookup String (Int -> b)
    | VarSet String Int b
    deriving Functor

data Free f a =
      Return a
    | Wrap (f (Free f a))

type Eval = Free EvalOp

instance Functor f => Functor (Free f) where
    fmap = liftM

instance Functor f => Applicative (Free f) where
    pure = return
    (<*>) = ap

instance Functor f => Monad (Free f) where

    return = Return

    Return x >>= cont = cont x
    Wrap c   >>= cont = Wrap (fmap (>>= cont) c)
                                   -- c :: f (Free f a) 
                                   -- cont :: a -> Free f b
                                   -- f (Free f b)
divByZeroError :: Eval a
divByZeroError = Wrap DivByZeroError

unknownVar :: String -> Eval a
unknownVar var = Wrap $ UnknownVar var

varLookup :: String -> Eval Int
varLookup var = Wrap $ VarLookup var return

varSet :: String -> Int -> Eval ()
varSet var n = Wrap $ VarSet var n (return ())

fromEval :: Eval a -> MT.Eval a
fromEval (Return a)               = return a
fromEval (Wrap DivByZeroError)    = MT.divisionByZero
fromEval (Wrap (UnknownVar var))  = MT.unknownVar var
fromEval (Wrap (VarLookup var f)) = MT.lookupVar var >>= fromEval . f
fromEval (Wrap (VarSet var n f))  = MT.assign var n >> fromEval f

--    return a >>= f
-- == Return a >>= f
-- == Bind (Return a) f
-- != f a
--
-- return x >>= f == f x
-- do                 do
--   x' <- return x     f x
--   f x'
--
-- a >>= return == a
--
-- do                 do
--   x <- a             a
--   return x
--
-- (a >>= f) >>= g == a >>= (\x -> f x >>= g)
--
-- do                 do
--   y <- do            x <- a
--          x <- a      y <- f x
--          f x         g y
--   g y











-- r :: a -> m a
-- w :: m a
--
-- data m a = r a | w












-- data Tree a = Leaf a | Node (Tree a) (Tree a)

data Tree a where

    Leaf :: a -> Tree a
    Node :: Tree a -> Tree a -> Tree a

data Expr :: * -> * where
    ILit :: Int -> Expr Int
    Add :: Expr Int -> Expr Int -> Expr Int
    BLit :: Bool -> Expr Bool
    IsZero :: Expr Int -> Expr Bool
   
deriving instance Show (Expr a)

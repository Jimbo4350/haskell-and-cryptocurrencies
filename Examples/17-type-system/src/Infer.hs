{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Control.Applicative hiding ((<|>))
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Syntax
import Types
import Repl

-- Free type variables and substitution

type Subst = Map Name MType

class Substitutable a where
  free :: a -> Set Name
  subst :: Subst -> a -> a

instance Substitutable MType where
  free :: MType -> Set Name
  free (TVar n)    = S.singleton n
  free (Con _)     = S.empty
  free (List t1)   = free t1
  free (Arr t1 t2) = free t1 `S.union` free t2

  subst :: Subst -> MType -> MType
  subst s (TVar n)    = M.findWithDefault (TVar n) n s
  subst _ (Con n)     = Con n
  subst s (List t1)   = List (subst s t1)
  subst s (Arr t1 t2) = Arr (subst s t1) (subst s t2)

instance Substitutable PType where
  free :: PType -> Set Name
  free (Forall xs t) = free t `S.difference` S.fromList xs

  subst :: Subst -> PType -> PType
  subst s (Forall xs t) = Forall xs (subst (foldr M.delete s xs) t)

instance Substitutable Env where
  free :: Env -> Set Name
  free gamma = S.unions (map free (M.elems gamma))

  subst :: Subst -> Env -> Env
  subst s gamma = M.map (subst s) gamma

-- | Identity substitution
identity :: Subst
identity = M.empty

-- | Composition of substitutions
comp :: Subst -> Subst -> Subst
comp s1 s2 = M.union s1 (M.map (subst s1) s2)

-- Actual inference

-- | This implements algorithm W.
--
-- Given an environment and an expression, we return
-- a substitution and a monotype. If the environment
-- contains no free type variables, the substitution
-- can be ignored.
--
infer :: Env -> Expr -> Infer (Subst, MType)

infer gamma (Var x) = do
  sigma <- lookupEnv x gamma
  tau <- inst sigma
  return (identity, tau)

infer gamma (App e1 e2) = do
  (s1, tau1) <- infer gamma e1
  (s2, tau2) <- infer (subst s1 gamma) e2
  tau3 <- fresh
  s3 <- unify (subst s2 tau1) (Arr tau2 tau3)
  return (s3 `comp` s2 `comp` s1, subst s3 tau3)

infer gamma (Lam x e) = do
  tau1 <- fresh
  (s, tau2) <- infer (M.insert x (Forall [] tau1) gamma) e
  return (s, Arr (subst s tau1) tau2)

infer gamma (Let x e1 e2) = do
  (s1, tau1) <- infer gamma e1
  let gamma' = subst s1 gamma
  let sigma = gen gamma' tau1
  (s2, tau2) <- infer (M.insert x sigma gamma') e2
  return (s2 `comp` s1, tau2)

-- | Unify two monotypes.
--
-- If successful, returns a substitution that makes
-- the two monotypes equal.
--
unify :: MType -> MType -> Infer Subst
unify (TVar n) t = bind n t
unify t (TVar n) = bind n t
unify (Con n1) (Con n2) | n1 == n2 = return identity
unify (Arr a1 a2) (Arr b1 b2) = do
  s1 <- unify a1 b1
  s2 <- unify (subst s1 a2) (subst s1 b2)
  return (s2 `comp` s1)
unify (List a) (List b) =
  unify a b
unify t1 t2 = cannotUnify t1 t2

-- | Helper function used in 'unify'.
--
-- Tries to bind the name to the given monotype.
-- Performs the "occurs check".
--
bind :: Name -> MType -> Infer Subst
bind n (TVar n') | n == n'     = return identity
bind n t | n `S.member` free t = occursCheckFailed n t
bind n t                       = return (M.singleton n t)

-- | Implements instantiation of a polytype.
inst :: PType -> Infer MType
inst (Forall xs t) = do
  assocs <- forM xs $ \ x -> do
    v <- fresh
    return (x, v)
  return (subst (M.fromList assocs) t)

-- | Generalises a monotype over a given environment.
gen :: Env -> MType -> PType
gen gamma tau = Forall xs tau
  where xs = S.toList (free tau `S.difference` free gamma)

-- The monad
--
-- We want the following operations:
--
-- data Infer a -- abstract
--
-- fresh :: Infer MType -- yields a fresh type variable
-- throwError :: String -> Infer a -- aborts with an error message

newtype Infer a =
  Infer (Int -> Either String (a, Int))

instance Functor Infer where
  fmap :: (a -> b) -> Infer a -> Infer b
  fmap = liftM

instance Applicative Infer where
  pure :: a -> Infer a
  pure = return

  (<*>) :: Infer (a -> b) -> Infer a -> Infer b
  (<*>) = ap

instance Monad Infer where
  return :: a -> Infer a
  return x = Infer (\ n -> Right (x, n))

  (>>=) :: Infer a -> (a -> Infer b) -> Infer b
  Infer f >>= k =
    Infer $ \ n ->
      case f n of
        Left err      -> Left err
        Right (x, n') ->
          case k x of
            Infer g -> g n'

-- | Obtains a fresh type variable.
fresh :: Infer MType
fresh = Infer $ \ n -> Right (TVar ("t" ++ show n), n + 1)

-- | Aborts inference with an error.
throwError :: String -> Infer a
throwError err = Infer $ \ _ -> Left err

-- Derived operations (in terms of throwError)

-- | Looks up a name in the environment.
--
-- Aborts if the name is not found.
--
lookupEnv :: Name -> Env -> Infer PType
lookupEnv n gamma =
  case M.lookup n gamma of
    Nothing -> throwError $ "unknown identifier: " ++ n
    Just t  -> return t

-- | Helper function used to report a unification error.
cannotUnify :: MType -> MType -> Infer a
cannotUnify t1 t2 =
  throwError $ "cannot unify " ++ pretty t1 ++ " and " ++ pretty t2

-- | Helper function used to report a failed occurs check.
occursCheckFailed :: Name -> MType -> Infer a
occursCheckFailed n t =
  throwError $ "occurs check failed for " ++ n ++ " in " ++ pretty t

-- Running the Infer monad

runInfer :: Infer a -> Either String a
runInfer (Infer f) = fst <$> f 0

typeOf :: Env -> Expr -> Either String PType
typeOf gamma e = runInfer $ do
  (_, t) <- infer gamma e
  return (gen gamma t)

-- Starting the interpreter main loop

initGamma :: Env
initGamma = M.fromList
  [ ("Zero", Forall [] intTy)
  , ("Suc", Forall [] (intTy .->. intTy))
  , ("caseNat", Forall ["a"] (intTy .->. TVar "a" .->. (intTy .->. TVar "a") .->. TVar "a"))
  , ("one", Forall [] intTy)
  , ("plus", Forall [] (intTy .->. intTy .->. intTy))
  , ("times", Forall [] (intTy .->. intTy .->. intTy))
  , ("True", Forall [] boolTy)
  , ("False", Forall [] boolTy)
  , ("caseBool", Forall ["a"] (boolTy .->. TVar "a" .->. TVar "a" .->. TVar "a"))
  , ("Nil", Forall ["a"] (List (TVar "a")))
  , ("Cons", Forall ["a"] (TVar "a" .->. List (TVar "a") .->. List (TVar "a")))
  , ("caseList", Forall ["a", "b"] (List (TVar "a") .->. TVar "b" .->.
                                     (TVar "a" .->. List (TVar "a") .->. TVar "b") .->. TVar "b"))
  , ("append", Forall ["a"] (List (TVar "a") .->. List (TVar "a") .->. List (TVar "a")))
  , ("length", Forall ["a"] (List (TVar "a") .->. intTy))
  , ("null", Forall ["a"] (List (TVar "a") .->. boolTy))
  , ("fix", Forall ["a"] ((TVar "a" .->. TVar "a") .->. TVar "a"))
  ]

main :: IO ()
main = repl initGamma typeOf

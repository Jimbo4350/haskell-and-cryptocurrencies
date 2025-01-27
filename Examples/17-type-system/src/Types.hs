module Types where

import Data.List
import Data.Map

import Syntax

-- Syntax of types

-- | Monotypes
--
-- Deviated from the slides in that we do not have
-- a general representation of n-ary type constructors.
--
-- We only have arbitrary nullary type constants, and
-- one unary type constructor (lists), for which we
-- implement special syntax, plus functions which we
-- need anyway.
--
data MType =
    TVar Name        -- inference (or quantified) variable
  | Con Name         -- nullary constants
  | List MType       -- list type constructor
  | Arr MType MType  -- function type
  deriving (Show)

-- | Polytypes / type schemes
data PType =
    Forall [Name] MType
  deriving (Show)

type Env = Map Name PType

-- Some syntactic sugar for more easily writing types
-- within Haskell.

intTy :: MType
intTy = Con "Int"

boolTy :: MType
boolTy = Con "Bool"

(.->.) :: MType -> MType -> MType
(.->.) = Arr

infixr 7 .->.
infixr `Arr`

-- Pretty-printing for types

class Pretty a where
  pretty :: a -> String

instance Pretty PType where
  pretty (Forall [] t) = pretty t
  pretty (Forall xs t) = "forall " ++ intercalate " " xs ++ " . " ++ pretty t

instance Pretty MType where
  pretty = go False
    where
      go :: Bool -> MType -> String
      go _ (TVar  i)   = i
      go _ (Con n)     = n
      go _ (List t)    = "[" ++ go False t ++ "]"
      go p (Arr t1 t2) =
        let
          b = go True t1 ++ " -> " ++ go False t2
        in
          if p then "(" ++ b ++ ")" else b

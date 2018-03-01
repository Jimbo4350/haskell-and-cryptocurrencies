{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Generic where

import Data.Binary  (Binary)
import GHC.Generics

data MyType a b =
      Flag Bool
    | Combo (a, a)
    | Other b Int (MyType a a)
    deriving (Generic, Binary)

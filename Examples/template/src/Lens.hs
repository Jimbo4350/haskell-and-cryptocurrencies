{-# LANGUAGE TemplateHaskell #-}

module Lens where

import Control.Lens    (set)
import Control.Lens.TH (makeLenses, makePrisms)

data Result a = Ok a | Error String deriving Show

newtype Company = Company { _staff   :: [Person] } deriving Show

data Person  = Person
    { _name    :: String
    , _address :: Address
    } deriving Show

newtype Address = Address { _city    :: String } deriving Show

marcin, lars :: Person
marcin = Person
  {  _name    = "Marcin"
  ,  _address = Address { _city = "Funchal" }
  }
lars = Person
  {  _name    = "Lars"
  ,  _address = Address { _city = "Regensburg" }
  }

makePrisms ''Result

makeLenses ''Company
makeLenses ''Person
makeLenses ''Address

iohk :: Company
iohk = Company { _staff = [marcin, lars] }

goTo :: String -> Company -> Company
goTo = set (staff . traverse . address . city)

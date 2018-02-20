module Data where

newtype Company = Company { _staff   :: [Person] } deriving Show

data Person  = Person
    { _name    :: String
    , _address :: Address
    } deriving Show

newtype Address = Address { _city    :: String } deriving Show

tamara, lars :: Person
tamara = Person
  {  _name    = "Tamara"
  ,  _address = Address { _city = "Toronto" }
  }
lars = Person
  {  _name    = "Lars"
  ,  _address = Address { _city = "Regensburg" }
  }

iohk :: Company
iohk = Company { _staff = [tamara, lars] }

data Result a = Ok a | Error String deriving Show

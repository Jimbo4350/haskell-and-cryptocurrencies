{-# LANGUAGE GADTs #-}

module HListEnv where

import GADT

envToHList :: Env xs I -> HList xs
envToHList Nil         = HNil
envToHList (I x :* xs) = x `HCons` envToHList xs


hListToEnv :: HList xs -> Env xs I
hListToEnv HNil           = Nil
hListToEnv (x `HCons` xs) = I x :* hListToEnv xs

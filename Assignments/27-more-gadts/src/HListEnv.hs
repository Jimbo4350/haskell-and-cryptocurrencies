{-# LANGUAGE GADTs #-}

module HListEnv where

import           GADT

envToHList :: Env xs I -> HList xs
envToHList = undefined


hListToEnv :: HList xs -> Env xs I
hListToEnv = undefined

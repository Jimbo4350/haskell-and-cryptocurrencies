{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Use where

import Template

mkProject 4 3

primes :: [Int]
primes = $(mkPrimes 1 100)

hundred :: Int
hundred = [ternary|10201|]

isHundred :: Int -> Bool
isHundred [ternary|10201|] = True
isHundred _                = False

module Main (main) where

import Test.DocTest (doctest)

main :: IO ()
main = doctest [ "src/Mini.hs"
               , "src/Nested.hs"
               ]

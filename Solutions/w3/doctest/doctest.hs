module Main (main) where

import Test.DocTest (doctest)

main :: IO ()
main = doctest [ "src/Distinctions.hs"
               , "src/FixedPoints.hs"
               , "src/Stack/Lex.hs"
               , "src/Stack/Parse.hs"
               ]

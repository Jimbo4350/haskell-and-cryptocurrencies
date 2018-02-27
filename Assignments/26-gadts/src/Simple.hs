{-# LANGUAGE OverloadedStrings #-}

module Simple where

import Data.Text (Text)

newtype Question = Q Text deriving Show

newtype Answer = A Bool deriving Show

exampleQ :: [Question]
exampleQ = [ Q "Do you like Haskell?"
           , Q "Do you like dynamic types?"
           ]

exampleA :: [Answer]
exampleA = [ A True
           , A False
           ]

type Score = Int

type Scoring = Answer -> Score

yesno :: Score -> Score -> Scoring
yesno yes no (A b) = if b then yes else no

exampleS :: [Scoring]
exampleS = [ yesno 5 0
           , yesno 0 2
           ]

score :: [Scoring] -> [Answer] -> Score
score ss as = sum (zipWith ($) ss as)


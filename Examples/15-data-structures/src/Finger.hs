module Finger where

newtype Seq a = Seq (FingerTree a)

data FingerTree a  =
      Empty
   |  Single a
   |  Deep (Digit a) (FingerTree (Node a)) (Digit a)


data Node a = Node2 a a
            | Node3 a a a

data Digit a =
  One a | Two a a | Three a a a | Four a a a a

cons :: a -> FingerTree a -> FingerTree a
cons a Empty                     = Single a
cons a (Single b)                = Deep (One a) Empty (One b)
cons a (Deep (One b) t r)        = Deep (Two a b) t r
cons a (Deep (Two b c) t r)      = Deep (Three a b c) t r
cons a (Deep (Three b c d) t r)  = Deep (Four a b c d) t r
cons a (Deep (Four b c d e) t r) = Deep (Two a b) (cons (Node3 c d e) t) r

empty :: FingerTree a
empty = Empty

singleton :: a -> FingerTree a
singleton = Single

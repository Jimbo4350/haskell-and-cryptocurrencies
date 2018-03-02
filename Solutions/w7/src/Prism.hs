module Prism where

import Numeric.Natural (Natural)
import Optics

_Natural :: Prism' Integer Natural
_Natural = prism' pr fromIntegral
  where
    pr n
        | n >= 0    = Just (fromIntegral n)
        | otherwise = Nothing

_TheOne :: Eq a => a -> Prism' a ()
_TheOne a = prism' pr (const a)
  where
    pr b
        | b == a    = Just ()
        | otherwise = Nothing

newtype Checked a = Checked {unChecked :: a} deriving Show

_Check :: (a -> Bool) -> Prism' a (Checked a)
_Check p = prism' pr unChecked
  where
    pr a
        | p a       = Just (Checked a)
        | otherwise = Nothing

filtering :: (a -> Bool) -> Traversal' s a -> Traversal' s a
filtering p t = t . _Check p . iso unChecked Checked

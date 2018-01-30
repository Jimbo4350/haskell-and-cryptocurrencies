{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Spec.FixedPointsSpec
    ( fixedPointsSpec
    ) where

import FixedPoints
import Test.Hspec
import Test.QuickCheck

fixedPointsSpec :: SpecWith ()
fixedPointsSpec = do
    mapSpec
    mysterySpec
    toTreeSpec

mapSpec :: SpecWith ()
mapSpec =
    describe "map'" $
        it "behaves like map" $ property mapProp

mysterySpec :: SpecWith ()
mysterySpec =
    describe "mystery" $
        it "behaves like iterate" $ property mysteryProp

toTreeSpec :: SpecWith ()
toTreeSpec =
    describe "toTree" $
        it "should be inverse to fromTree" $ property fromToTreeProp

newtype IntInt = IntInt (Int -> Int)
    deriving Arbitrary

instance Show IntInt where

    show _ = "[Int->Int]"

mapProp :: IntInt -> [Int] -> Property
mapProp (IntInt f) xs = map' f xs === map f xs

mysteryProp :: IntInt -> Int -> Int -> Property
mysteryProp (IntInt f) n x = take n (mystery f x) === take n (iterate f x)

fromToTreeProp :: Tree Char -> Property
fromToTreeProp t = toTree (fromTree t) === t

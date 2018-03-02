{-# LANGUAGE RankNTypes #-}

import Foldr
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "replicate'" $
        it "should behave like replicate" (property replicateProp)
    describe "fromTo" $
        it "should return the correct range of ints" (property fromToProp)
    describe "foldr-build fusion law" $ do
        it "should hold for summing and the replicateBuilder" (property sumReplicateProp)
        it "should hold for product and the fromToBuilder" (property sumFromToProp)
        it "should hold for show/concat and the fromToBuilder" (property showFromToProp)

replicateProp :: Int -> Char -> Property
replicateProp n c = replicate' n c === replicate n c

fromToProp :: Int -> Int -> Property
fromToProp a b = fromTo a b === [a, a + 1 .. b]

foldrBuilderProp :: (Eq a, Show a) => a -> (Int -> a -> a) -> (forall r. r -> (Int -> r -> r) -> r) -> Property
foldrBuilderProp e op builder = foldr op e (build builder) === builder e op

sumReplicateProp :: Int -> Int -> Property
sumReplicateProp m n = foldrBuilderProp 0 (+) (replicateBuilder m n)

sumFromToProp :: Int -> Int -> Property
sumFromToProp a b = foldrBuilderProp 0 (+) (fromToBuilder a b)

showFromToProp :: Int -> Int -> Property
showFromToProp a b = foldrBuilderProp "" (\n s -> show n ++ s) (fromToBuilder a b)

import Nested
import Prelude         hiding (reverse)
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "reverse" $
        it "should be an involution" $ property reverseProp
    describe "index" $
        it "should be correct" $ property indexBuildProp

-- |Property stating that @'reverse'@ is an involution.
reverseProp :: Perfect Int -> Property
reverseProp p = reverse (reverse p) === p

-- |Property stating that first using @'build'@ to build a perfect tree
-- and then using @'index'@ is the same as indexing in the list used for
-- building (as long as the list is long enough and the index is in range).
indexBuildProp :: Property
indexBuildProp = forAll (genListDepthIndex :: Gen ([Int], Int, Int)) $ \(xs, d, i) ->
    index (build d xs) i === Just (xs !! i)

genListDepthIndex :: Arbitrary a => Gen ([a], Int, Int)
genListDepthIndex = do
    x   <- arbitrary
    xs' <- arbitrary
    let xs = x : xs' -- non-empty list
        d  = last $ takeWhile (\d' -> 2^d' <= length xs) [0..]
    i <- elements [0.. 2^d - 1]
    return (xs, d, i)

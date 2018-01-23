import Data.List       (foldl')
import Data.Maybe      (fromJust)
import Prelude         hiding (null, lookup)
import Test.Hspec
import Test.QuickCheck

import Tries

main :: IO ()
main = hspec $ do
  describe "empty" $ do
    it "is valid" prop_empty_valid
  describe "genValidTrie" $
    it "generates valid tries" prop_genValidTrie_valid
  describe "insert" $ do
    it "maintains validity" $ property prop_insert_maintains_validity
    it "is idempotent" $ property prop_insert_idempotent
    it "is commutative" $ property prop_insert_comm
    it "increases the length by zero or one" $ property prop_insert_length
    it "reverts delete" prop_delete_insert
  describe "delete" $ do
    it "maintains validity" prop_delete_maintains_validity
    it "is idempotent" prop_delete_idempotent
    it "is commutative" prop_delete_comm
    it "decreases the length by zero or one" $ property prop_delete_length
    it "reverts insert" $ property prop_insert_delete
    it "gives the empty trie after applying it to all keys" prop_delete_all
  describe "lookup" $ do
    it "finds inserted keys" $ property prop_lookup_insert
    it "does not find deleted keys" $ property prop_lookup_delete

prop_empty_valid :: Bool
prop_empty_valid = valid empty

prop_genValidTrie_valid :: Property
prop_genValidTrie_valid = forAll (genValidTrie :: Gen (Trie Char Int)) valid

prop_insert_maintains_validity :: String -> Int -> Property
prop_insert_maintains_validity s n =
  forAll genValidTrie $ \t -> valid (insert s n t)

prop_insert_idempotent :: String -> Int -> Property
prop_insert_idempotent s n = forAll (genValidTrie :: Gen (Trie Char Int)) $ \t ->
  let t' = insert s n t
  in  t' === insert s n t'

prop_insert_comm :: String -> Int -> String -> Int -> Property
prop_insert_comm s n s' n' = forAll (genValidTrie :: Gen (Trie Char Int)) $ \t ->
  s /= s' ==> insert s n (insert s' n' t) === insert s' n' (insert s n t)

prop_insert_length :: String -> Int -> Trie Char Int -> Property
prop_insert_length s n t =
  let m  = member s t
      l  = length t
      l' = if m then l else l + 1
  in  collect m $ length (insert s n t) === l'

prop_delete_maintains_validity :: Property
prop_delete_maintains_validity =
  forAll (genValidTrieAndKey :: Gen (Trie Char Int, String)) $ \(t, s) ->
    valid (delete s t)

prop_delete_idempotent :: Property
prop_delete_idempotent =
  forAll (genValidTrieAndKey :: Gen (Trie Char Int, String)) $ \(t, s) ->
    let t' = delete s t
    in  t' === delete s t'

prop_delete_comm :: Property
prop_delete_comm =
  forAll (genValidTrieAndKeys :: Gen (Trie Char Int, String, String)) $ \(t, s, s') ->
    delete s (delete s' t) === delete s' (delete s t)

prop_delete_length :: String -> Trie Char Int -> Property
prop_delete_length s t =
  let m  = member s t
      l  = length t
      l' = if m then l - 1 else l
  in  collect m $ length (delete s t) === l'

prop_insert_delete :: String -> Int -> Property
prop_insert_delete s n = forAll genValidTrie $ \t ->
  not (member s t) ==> delete s (insert s n t) === t

prop_delete_insert :: Property
prop_delete_insert = forAll (genValidTrieAndKey :: Gen (Trie Char Int, String)) $ \(t, s) ->
  let n = fromJust (lookup s t) :: Int
  in  insert s n (delete s t) === t

prop_delete_all :: Property
prop_delete_all = forAll (genValidTrie :: Gen (Trie Char Int)) $ \t ->
  foldl' (flip delete) t (keys t) === empty

prop_lookup_insert :: String -> Int -> Property
prop_lookup_insert s n = forAll genValidTrie $ \t ->
  lookup s (insert s n t) === Just n

prop_lookup_delete :: String -> Property
prop_lookup_delete s = forAll (genValidTrie :: Gen (Trie Char Int)) $ \t ->
  lookup s (delete s t) === Nothing

genNonEmptyValidTrie :: (Ord a, Arbitrary a, Arbitrary b) => Gen (Trie a b)
genNonEmptyValidTrie = do
  t <- scale (+ 1) genValidTrie
  if (null t)
    then genNonEmptyValidTrie
    else return t

genValidTrieAndKey :: (Ord a, Arbitrary a, Arbitrary b) => Gen (Trie a b, [a])
genValidTrieAndKey = do
  t <- genNonEmptyValidTrie
  k <- elements $ keys t
  return (t, k)

genValidTrieAndKeys :: (Ord a, Arbitrary a, Arbitrary b) => Gen (Trie a b, [a], [a])
genValidTrieAndKeys = do
  t  <- genNonEmptyValidTrie
  k  <- elements $ keys t
  k' <- elements $ keys t
  return (t, k, k')

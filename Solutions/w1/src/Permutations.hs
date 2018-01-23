module Permutations
  ( allPicks
  , permutations
  ) where

-- |Given a list, return all possibilities to pick one element
-- from the list and return that element and the other elements.
allPicks :: [a] -> [(a, [a])]
allPicks []       = [] -- The empty list has no element, so there is no way to pick one.
allPicks (x : xs) =
  (x, xs) :                              -- We can pick the head...
  [(y, x : ys) | (y, ys) <- allPicks xs] -- ..or not, in which case we can look at all
                                         -- picks of the tail.
-- |Gets all permutations of a list.
permutations :: [a] -> [[a]]
permutations [] = [[]]         -- The empty list has exactly one permutation: itself.
permutations xs = do           -- For a non-empty list, to get any permutation,
    (y, ys) <- allPicks xs     -- we pick one element as head of the permutation
    zs      <- permutations ys -- and one permutation of the other elements as tail.
    return $ y : zs

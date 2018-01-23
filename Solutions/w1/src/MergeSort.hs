module MergeSort
  ( split
  , merge
  , mergeSort
  ) where

-- |Splits one list into two lists of approximately equal size.
split :: [a] -> ([a], [a])
split []           = ([], [])
split [x]          = ([x], [])
split (x : y : zs) =
  let (xs, ys) = split zs
  in  (x : xs, y : ys)

-- |Merge two sorted(!) lists.
merge :: Ord a => [a] -> [a] -> [a]
merge []           ys         = ys
merge xs           []         = xs
merge xs@(x : xs') ys@(y : ys')
  | x <= y                    = x : merge xs' ys
  | otherwise                 = y : merge xs ys'

-- |Sort a list by splitting the list into two parts,
-- recursively sorting those parts
-- and then merging the results.
mergeSort :: Ord a => [a] -> [a]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs =
  let (ys, zs) = split xs
  in  merge (mergeSort ys) (mergeSort zs)

module Chains where

-- These are example solutions to the first set of assignments.

data Chain txs =
    GenesisBlock
  | Block (Chain txs) txs
  deriving (Show)

eqChain :: Eq txs => Chain txs -> Chain txs -> Bool
eqChain GenesisBlock    GenesisBlock    = True
eqChain (Block c1 txs1) (Block c2 txs2) = eqChain c1 c2 && txs1 == txs2
eqChain _               _               = False

instance Eq txs => Eq (Chain txs) where
  (==) = eqChain

(|>) :: Chain txs -> txs -> Chain txs
(|>) = Block

infixl 5 |>

chain1 :: Chain Int
chain1 = GenesisBlock |> 2

chain2 :: Chain Int
chain2 = chain1 |> 4

chain3 :: Chain Int
chain3 = GenesisBlock |> 2 |> 8 |> 3

chain4 :: Chain Int
chain4 = GenesisBlock |> 2 |> 8 |> 4

chains :: [Chain Int]
chains = [chain1, chain2, chain3, chain4]

-- Task Chains-1.
--
-- Compute the length of a 'Chain'.

lengthChain :: Chain txs -> Int
lengthChain GenesisBlock = 0
lengthChain (Block c _)  = 1 + lengthChain c

propLengthChain1 :: Bool
propLengthChain1 = lengthChain chain1 == 1

propLengthChain2 :: Bool
propLengthChain2 = lengthChain chain2 == 2

propLengthChain3 :: Bool
propLengthChain3 = lengthChain chain3 == 3

propLengthChain4 :: Bool
propLengthChain4 = lengthChain chain4 == 3

-- Same as the above four properties in a single property.
propLengthChain5 :: Bool
propLengthChain5 =
  map lengthChain chains == [1, 2, 3, 3]

-- Task Chains-2.
--
-- Sum all entries in an integer chain.

sumChain :: Chain Int -> Int
sumChain GenesisBlock = 0
sumChain (Block c t)  = t + sumChain c

propSumChain1 :: Bool
propSumChain1 = sumChain chain1 == 2

propSumChain2 :: Bool
propSumChain2 = sumChain chain2 == 6

propSumChain3 :: Bool
propSumChain3 = sumChain chain3 == 13

propSumChain4 :: Bool
propSumChain4 = sumChain chain4 == 14

-- Same as the above four properties in a single property.
propSumChain5 :: Bool
propSumChain5 =
  map sumChain chains == [2, 6, 13, 14]

-- Task Chains-3.
--
-- Find the maximum element in an integer chain.
-- You can assume for this tasks that all integers
-- in a chain are positive, and that the maximum
-- of an empty chain is 0.

maxChain :: Chain Int -> Int
maxChain GenesisBlock = 0
maxChain (Block c t) = max t (maxChain c)

propMaxChain :: Bool
propMaxChain =
  map maxChain chains == [2, 4, 8, 8]

-- Task Chains-4.
--
-- Return the longer of two chains.
-- If both chains have the same length, return
-- the first.

longerChain :: Chain txs -> Chain txs -> Chain txs
longerChain c d = if lengthChain c >= lengthChain d then c else d

propLongerChain1 :: Bool
propLongerChain1 = longerChain chain1 chain2 == chain2

propLongerChain2 :: Bool
propLongerChain2 = longerChain chain2 chain1 == chain2

propLongerChain3 :: Bool
propLongerChain3 = longerChain chain2 chain3 == chain3

propLongerChain4 :: Bool
propLongerChain4 = longerChain chain3 chain4 == chain3

propLongerChain5 :: Bool
propLongerChain5 = and [ propLongerChain1
                       , propLengthChain2
                       , propLengthChain3
                       , propLengthChain4
                       ]

-- Task Chains-5.
--
-- Let's call an integer chain "valid" if from the genesis
-- block, each transaction has a higher number than all
-- preceding transactions. (You may assume that all integers
-- are positive.) Check that a given chain is valid.

validChain :: Chain Int -> Bool
validChain GenesisBlock           = True
validChain (Block GenesisBlock _) = True
validChain (Block (Block c t) t') = t' > t && validChain (Block c t)

propValidChain1 :: Bool
propValidChain1 = validChain GenesisBlock

propValidChain2 :: Bool
propValidChain2 =
  map validChain chains == [True, True, False, False]

-- Task Chains-6.
--
-- Given two chains, find out whether the first is a prefix
-- of the second. If two chains are equal, they still count
-- as a prefix of each other.
--
-- TODO: If you want to solve this in an efficient way,
-- it is actually much more tricky than the other exercises
-- up to this point. The length version is less obvious.

isPrefixOf :: Eq txs => Chain txs -> Chain txs -> Bool
isPrefixOf c d = isPrefixOf'
    (reverse (toList c))
    (reverse (toList d))

toList :: Chain txs -> [txs]
toList GenesisBlock = []
toList (Block c t)  = t : toList c

isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' []       _        = True
isPrefixOf' (_ : _)  []       = False
isPrefixOf' (x : xs) (y : ys) = if x == y then isPrefixOf' xs ys
                                          else False

propIsPrefixOf1 :: Bool
propIsPrefixOf1 = isPrefixOf chain1 chain2

propIsPrefixOf2 :: Bool
propIsPrefixOf2 = not (isPrefixOf chain2 chain1)

propIsPrefixOf3 :: Bool
propIsPrefixOf3 = isPrefixOf chain2 chain2

propIsPrefixOf4 :: Bool
propIsPrefixOf4 = not (isPrefixOf chain3 chain4)

-- The genesis block is a prefix of any chain.
propIsPrefixOf5 :: Bool
propIsPrefixOf5 =
  all (GenesisBlock `isPrefixOf`) chains

propIsPrefixOf6 :: Bool
propIsPrefixOf6 = and [ propIsPrefixOf1
                      , propIsPrefixOf2
                      , propIsPrefixOf3
                      , propIsPrefixOf4
                      , propIsPrefixOf5
                      ]

-- Task Chains-7.
--
-- Given two chains, find out whether one is a prefix of the
-- other.

areCompatible :: Eq txs => Chain txs -> Chain txs -> Bool
areCompatible c d = isPrefixOf c d || isPrefixOf d c

propAreCompatible1 :: Bool
propAreCompatible1 = areCompatible chain1 chain2

propAreCompatible2 :: Bool
propAreCompatible2 = areCompatible chain2 chain1

propAreCompatible3 :: Bool
propAreCompatible3 = not (areCompatible chain3 chain4)

propAreCompatible4 :: Bool
propAreCompatible4 = not (areCompatible chain4 chain3)

-- The genesis block is compatible with any chain.
propAreCompatible5 :: Bool
propAreCompatible5 =
  all (areCompatible GenesisBlock) chains

-- All chains are compatible with the genesis block.
propAreCompatible6 :: Bool
propAreCompatible6 =
  all (\ c -> areCompatible c GenesisBlock) chains

propAreCompatible7 :: Bool
propAreCompatible7 = and [ propAreCompatible1
                         , propAreCompatible2
                         , propAreCompatible3
                         , propAreCompatible4
                         , propAreCompatible5
                         , propAreCompatible6
                         ]

-- Task Chains-8.
--
-- Given two chains, find the longest common prefix.

commonPrefix :: Eq txs => Chain txs -> Chain txs -> Chain txs
commonPrefix c d = fromList (reverse (commonPrefix'
    (reverse (toList c))
    (reverse (toList d))))

fromList :: [txs] -> Chain txs
fromList [] = GenesisBlock
fromList (t : ts) = Block (fromList ts) t

commonPrefix' :: Eq a => [a] -> [a] -> [a]
commonPrefix' []       _        = []
commonPrefix' _        []       = []
commonPrefix' (x : xs) (y : ys) = if x == y then x : commonPrefix' xs ys
                                            else []

propCommonPrefix1 :: Bool
propCommonPrefix1 = commonPrefix chain1 chain2 == chain1

propCommonPrefix2 :: Bool
propCommonPrefix2 = commonPrefix chain2 chain1 == chain1

propCommonPrefix3 :: Bool
propCommonPrefix3 = commonPrefix chain1 chain3 == chain1

propCommonPrefix4 :: Bool
propCommonPrefix4 = commonPrefix chain3 chain4 == chain1 |> 8

propCommonPrefix5 :: Bool
propCommonPrefix5 =
  commonPrefix chain3 (GenesisBlock |> 5) == GenesisBlock

propCommonPrefix6 :: Bool
propCommonPrefix6 = and [ propCommonPrefix1
                        , propCommonPrefix2
                        , propCommonPrefix3
                        , propCommonPrefix4
                        , propCommonPrefix5
                        ]

-- Task Chains-9.
--
-- Reimplement the hasBlockProp function from the slides
-- for our more general Chain type which is polymorphic
-- in the type of transactions.

hasBlockProp :: (txs -> Bool) -> Chain txs -> Bool
hasBlockProp _    GenesisBlock = False
hasBlockProp prop (Block c t)  = prop t || hasBlockProp prop c

-- Task Chains-10.
--
-- Reimplement hasBlock in terms of hasBlockProp.

hasBlock :: Eq txs => txs -> Chain txs -> Bool
hasBlock t = hasBlockProp (== t)

-- Task Chains-11.
--
-- Check whether all blocks in a chain are unique,
-- i.e., different from each other.

uniqueBlocks :: Eq txs => Chain txs -> Bool
uniqueBlocks GenesisBlock = True
uniqueBlocks (Block c t)  = uniqueBlocks c && not (hasBlock t c)

-- Task Chains-12.
--
-- Check whether all blocks in the given chain have
-- a particular property.

allBlockProp :: (txs -> Bool) -> Chain txs -> Bool
allBlockProp _    GenesisBlock = True
allBlockProp prop (Block c t)  = prop t && allBlockProp prop c

-- Task Chains-13.
--
-- Given a list of chains, determine the maximum length.
-- If the given list is empty, return 0.

maxChains :: [Chain txs] -> Int
maxChains []       = 0
maxChains (c : cs) = max (lengthChain c) (maxChains cs)

-- Task Chains-14.
--
-- Given a non-empty list of chains, determine the longest
-- common prefix. We model a non-empty list here as a single
-- element plus a normal list.

longestCommonPrefix :: Eq txs => Chain txs -> [Chain txs] -> Chain txs
longestCommonPrefix c []        = c
longestCommonPrefix c (c' : cs) = commonPrefix c (longestCommonPrefix c' cs)

-- Task Chains-15.
--
-- Given an integer chain, interpret each integer as a change
-- of the current balance. The genesis block has a balance of 0.
-- The final balance is given by sumChain. Define a function
-- that computes a chain of all the intermediate balances. The
-- resulting chain should have the same length as the original
-- chain, but each entry should be the intermediate balance of
-- the original chain at that point.

balancesChain :: Chain Int -> Chain Int
balancesChain GenesisBlock = GenesisBlock
balancesChain (Block c t)  = case balancesChain c of
    GenesisBlock -> Block GenesisBlock t
    Block c' t'  -> Block (Block c' t') (t + t')

propBalancesChain1 :: Bool
propBalancesChain1 =
  balancesChain chain1 == chain1

propBalancesChain2 :: Bool
propBalancesChain2 =
  balancesChain chain2 == chain1 |> 6

propBalancesChain3 :: Bool
propBalancesChain3 =
  balancesChain chain3 == chain1 |> 10 |> 13

propBalancesChain4 :: Bool
propBalancesChain4 =
  balancesChain chain4 == chain1 |> 10 |> 14

chain5 :: Chain Int
chain5 = GenesisBlock |> 5 |> (-5)

chain6 :: Chain Int
chain6 = chain5 |> (-1) |> 3

propBalancesChain5 :: Bool
propBalancesChain5 =
  balancesChain chain5 == GenesisBlock |> 5 |> 0

propBalancesChain6 :: Bool
propBalancesChain6 =
  balancesChain chain6 == GenesisBlock |> 5 |> 0 |> (-1) |> 2

propBalancesChain7 :: Bool
propBalancesChain7 = and [ propBalancesChain1
                         , propBalancesChain2
                         , propBalancesChain3
                         , propBalancesChain4
                         , propBalancesChain5
                         , propBalancesChain6
                         ]

-- Task Chains-16.
--
-- Given an integer chain, interpret it as a balances chain
-- as in the previous task and check that none of the
-- intermediate balances are negative.

validBalancesChain :: Chain Int -> Bool
validBalancesChain GenesisBlock = True
validBalancesChain (Block c _)  = allBlockProp (>= 0) c

propValidBalancesChain1 :: Bool
propValidBalancesChain1 =
  all validBalancesChain [chain1, chain2, chain3, chain4, chain5]

propValidBalancesChain2 :: Bool
propValidBalancesChain2 =
  not (validBalancesChain chain6)

propValidBalancesChain3 :: Bool
propValidBalancesChain3 = and [ propValidBalancesChain1
                              , propValidBalancesChain2
                              ]

-- Task Chains-17.
--
-- Drop blocks from the end of the chain as long as the
-- transactions in the blocks fulfill the given property.
-- Return the rest.

shortenWhile :: (txs -> Bool) -> Chain txs -> Chain txs
shortenWhile _    GenesisBlock = GenesisBlock
shortenWhile prop (Block c t)  = if prop t then shortenWhile prop c
                                           else Block c t

-- Task Chains-18.
--
-- Reimplement the function 'build' from the slides.

build :: Int -> Chain Int
build n = if n <= 0 then GenesisBlock
                    else Block (build (n - 1)) n

-- Task Chains-19.
--
-- Produce a chain of given length that contains the
-- given transactions in every block.
--
-- If the given length is zero or negative, return the
-- genesis block.

replicateChain :: Int -> txs -> Chain txs
replicateChain l t = if l <= 0 then GenesisBlock
                               else Block (replicateChain (l - 1) t) t

-- Task Chains-20.
--
-- Implement a function that gives you the prefix of the
-- given length of the given chain. If the chain is too short,
-- the entire chain is returned. If the given length is negative,
-- return the genesis block only.

cutPrefix :: Int -> Chain txs -> Chain txs
cutPrefix n c = fromList (reverse (cutPrefix' n (reverse (toList c))))

cutPrefix' :: Int -> [a] -> [a]
cutPrefix' n xs = if n <= 0 then []
                            else case xs of
                                []        -> []
                                (x : xs') -> x : cutPrefix' (n - 1) xs'

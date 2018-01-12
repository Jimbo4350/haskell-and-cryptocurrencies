module Transactions where

import Prelude hiding (lookup)
import Tables

-- START HERE AFTER Tables.hs

-- This is the transactions datatype from the slides,
-- using record syntax.

data Transaction =
  Transaction
    { trAmount :: Amount
    , trFrom   :: Account
    , trTo     :: Account
    }
  deriving (Eq, Show)

type Amount  = Int
type Account = String

-- Both declarations below define transactions. The
-- first uses the 'Transaction' constructor normally,
-- the second uses record syntax during construction.
--
-- In the record version, we can assign the fields
-- in a different oder; otherwise, both versions are
-- equivalent.

transaction1 :: Transaction
transaction1 = Transaction 10 "Andres" "Lars"

transaction2 :: Transaction
transaction2 =
  Transaction
    { trAmount = 7
    , trFrom   = "Lars"
    , trTo     = "Philipp"
    }

-- Task Transactions-1.
--
-- Flip a transaction, by producing a transaction of
-- the negative amount in the opposite direction.

flipTransaction :: Transaction -> Transaction
flipTransaction t = Transaction
    { trAmount = - (trAmount t)
    , trFrom   = trTo t
    , trTo     = trFrom t
    }

-- Task Transactions-2.
--
-- Normalize a transaction by flipping it if and only
-- if the transaction amount is negative.

normalizeTransaction :: Transaction -> Transaction
normalizeTransaction t
    | trAmount t < 0 = flipTransaction t
    | otherwise      = t

-- Task Transactions-3.
--
-- Re-implement 'processTransaction' from the slides,
-- but use the function 'alter' that you defined in
-- the Tables tasks.

type Accounts = Table Account Amount

processTransaction :: Transaction -> Accounts -> Accounts
processTransaction (Transaction a f t) as =
    alter (alterAcc (- a))
          f
          (alter (alterAcc a) t as)

-- A helper function for use with 'alter'.
alterAcc :: Amount -> Maybe Amount -> Maybe Amount
alterAcc d Nothing  = Just d
alterAcc d (Just x) = Just (x + d)

-- Task Transactions-4.
--
-- Verify that you can no longer pattern match on the
-- 'Tables' constructor if you have hidden the 'Tables'
-- constructor from the export list as requested in the
-- Tables tasks.

-- Task Transactions-5.
--
-- Process a list of transactions one by one.

processTransactions :: [Transaction] -> Accounts -> Accounts
processTransactions []       as = as
processTransactions (t : ts) as = processTransactions ts (processTransaction t as)

-- Task Transactions-6.
--
-- Write a version of 'processTransaction' that fails
-- if and only if the new balances would be negative.

processTransaction' :: Transaction -> Accounts -> Maybe Accounts
processTransaction' tr@(Transaction _ f t) as =
    let as'  = processTransaction tr as
    in  case (lookup f as', lookup t as') of
            (Just newF, Just newT)
                | newF < 0 || newT < 0 -> Nothing
            _                          -> Just as'

-- Task Transactions-7.
--
-- Write a versionof 'processTransactions' that fails
-- if any of the individual transactions fail.

processTransactions' :: [Transaction] -> Accounts -> Maybe Accounts
processTransactions' [] as       = Just as
processTransactions' (t : ts) as = case processTransaction' t as of
    Nothing  -> Nothing
    Just as' -> processTransactions' ts as'

-- Task Transactions-8.
--
-- Create a package (with a Cabal, and optionally a stack
-- file) out of this set of assignments. Make sure you can
-- build the package using either stack or cabal-install.

-- Task Transactions-9.
--
-- Make your package depend on the @containers@ package.
-- For this module, switch from using the 'Tables' datatype
-- to the 'Map' datatype from the 'Data.Map' module, and
-- verify that everything still works.

-- GO BACK to Datatypes.hs

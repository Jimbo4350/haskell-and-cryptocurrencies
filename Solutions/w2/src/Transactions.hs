module Transactions where

import           Control.Monad
import           Data.List     (foldl')
import           Data.Map      (Map)
import qualified Data.Map      as M

data Transaction = Transaction
    { tId      :: Id
    , tInputs  :: [Input]
    , tOutputs :: [Output]
    } deriving Show

type Id = Int

-- |A transaction output.
--
-- Note that I derived @`Eq`@ in addition to @`Show`@,
-- because that helps with testing.
data Output = Output
    { oValue   :: Int
    , oAddress :: Address
    } deriving (Show, Eq)

type Address = String

-- |A transaction input.
data Input = Input
    { iPrevious :: Id
    , iIndex    :: Index
    } deriving (Show, Eq, Ord)

type Index = Int

type UTXOs = Map Input Output

-- |Processes one transaction:
--
--   1. Processes all inputs, deletes them from the @UTXOs@ and calculates the
--   sum of their values.
--
--   2. Processes all outputs, inserts them into the @UTXOs@ and calculates
--   the sum of their values.
--
--   3. Checks whether the sum of input values is at least as large as the sum
--   of output values.
--
-- If all three steps succeed, the updated @UTXOs@ are returned as a @`Right`@;
-- otherwise an error is reported as a @`Left`@.
processTransaction :: Transaction -> UTXOs -> Either String UTXOs
processTransaction Transaction{..} utxos = do
    (inputValue, utxos') <- processInputs tId tInputs utxos
    let (outputValue, utxos'') = processOutputs tId tOutputs utxos'
    when (inputValue < outputValue) $
        Left $ "input value " ++ show inputValue ++ 
               " less than output value " ++ show outputValue ++ 
               " in transaction " ++ show tId
    return utxos''

-- |Processes a list of transactions by simply invoking @`processTransaction`@
-- for each transaction in turn, failing if this fails for one transaction,
-- otherwise passing the updated @UTXOs@ to the next transaction.
-- If all transactions can be processed, the final @UTXOs@ are returned.
processTransactions :: [Transaction] -> UTXOs -> Either String UTXOs
processTransactions txs utxos = foldM (flip processTransaction) utxos txs

-- |Processes one input. Returns a @`Left`@ if the input does not exist;
-- deletes it from the @UTXOs@ and returns its value as a @`Right`@ if it does.
processInput :: Id -> Input -> UTXOs -> Either String (Int, UTXOs)
processInput txId input utxos = case M.lookup input utxos of
    Nothing         -> Left $ "non-existent input " ++ show input ++ " in transaction " ++ show txId
    Just Output{..} -> return (oValue, M.delete input utxos)

-- |Processes a list of inputs.
-- Returns a @`Left`@ if any of the inputs does not exist;
-- deletes all inputs from the @UTXOs@ and returns the
-- sum of their values as a @`Right`@ otherwise.
--
-- Note that this automatically checks for duplicate inputs,
-- because if there indeed is such a duplicate,
-- then it will have been removed from the @UTXOs@ the first
-- time it was encountered, thus failing the second time.
processInputs :: Id -> [Input] -> UTXOs -> Either String (Int, UTXOs)
processInputs txId inputs utxos = foldM f (0, utxos) inputs
  where
    f :: (Int, UTXOs) -> Input -> Either String (Int, UTXOs)
    f (acc, utxos') input = do
        (value, utxos'') <- processInput txId input utxos'
        return (acc + value, utxos'')

-- |Processes one output by inserting it into the @UTXOs@.
-- Returns the updated @UTXOs@.
processOutput :: Id -> Index -> Output -> UTXOs -> UTXOs
processOutput txId i output utxos =
    let input = Input {iPrevious = txId, iIndex = i}
    in  M.insert input output utxos

-- |Processes a list of outputs by inserting them into the @UTXOs@.
-- Returns the sum of their values and the updated @UTXOs@.
processOutputs :: Id -> [Output] -> UTXOs -> (Int, UTXOs)
processOutputs txId outputs utxos = foldl' f (0, utxos) $ zip [0..] outputs
  where
    f :: (Int, UTXOs) -> (Int, Output) -> (Int, UTXOs)
    f (acc, utxos') (i, output) = (acc + oValue output, processOutput txId i output utxos')

newtype ErrorState s a = ErrorState {runErrorState :: s -> Either String (a, s)}

instance Functor (ErrorState s) where

    fmap = liftM

instance Applicative (ErrorState s) where

    pure = return

    (<*>) = ap

instance Monad (ErrorState s) where

    return a = ErrorState $ \s -> Right (a, s)

    m >>= cont = ErrorState $ \s -> case runErrorState m s of
        (Left e)        -> Left e
        (Right (a, s')) -> runErrorState (cont a) s'

throwError :: String -> ErrorState s a
throwError e = ErrorState $ \_ -> Left e

get :: ErrorState s s
get = ErrorState $ \s -> Right (s, s)

put :: s -> ErrorState s ()
put s = ErrorState $ \_ -> Right ((), s)

-- |Convenience function for the common case of @`get`@ting the state,
-- applying a function to it and @`put`@ting the new state.
modify :: (s -> s) -> ErrorState s ()
modify f = get >>= \s -> put (f s)

-- |@`ErrorState`@-version of @`processTransaction`@.
processTransactionES :: Transaction -> ErrorState UTXOs ()
processTransactionES Transaction{..} = do
    inputValue  <- processInputsES tId tInputs
    outputValue <- processOutputsES tId tOutputs
    when (inputValue < outputValue) $
        throwError $ "input value " ++ show inputValue ++ 
                     " less than output value " ++ show outputValue ++ 
                     " in transaction " ++ show tId

-- |Same behavior as @`processTransaction`@, but using @`processTransactions'`@
-- (and hence @`ErrorState`@) "under the hood".
processTransaction' :: Transaction -> UTXOs -> Either String UTXOs
processTransaction' tx = processTransactions' [tx]

-- |@`ErrorState`@-version of @`processTransactions`@.
processTransactionsES :: [Transaction] -> ErrorState UTXOs ()
processTransactionsES = mapM_ processTransactionES

-- |Same behavior as @`processTransactions`@, but using @`processTransactionsES`@
-- (and hence @`ErrorState`@) "under the hood".
processTransactions' :: [Transaction] -> UTXOs -> Either String UTXOs
processTransactions' txs utxos = snd <$> runErrorState (processTransactionsES txs) utxos

-- |@`ErrorState`@-version of @`processInput`@.
processInputES :: Id -> Input -> ErrorState UTXOs Int
processInputES txId input = do
    utxos <- get
    case M.lookup input utxos of
        Nothing         -> throwError $ "non-existent input " ++ show input ++ " in transaction " ++ show txId
        Just Output{..} -> do
            put $ M.delete input utxos
            return oValue

-- |@`ErrorState`@-version of @`processInputs`@.
processInputsES :: Id -> [Input] -> ErrorState UTXOs Int
processInputsES txId inputs = sum <$> mapM (processInputES txId) inputs

-- |@`ErrorState`@-version of @`processOutput`@.
-- Returns the output value.
processOutputES :: Id -> Index -> Output -> ErrorState UTXOs Int
processOutputES txId i output = do
    modify $ M.insert (Input txId i) output
    return $ oValue output

-- |@`ErrorState`@-version of @`processOutputs`@.
processOutputsES :: Id -> [Output] -> ErrorState UTXOs Int
processOutputsES txId outputs = sum <$> zipWithM (processOutputES txId) [0..] outputs

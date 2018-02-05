import qualified Data.Map as M
import           Test.Hspec
import           Transactions

main :: IO ()
main = hspec $ do
    describe "processTransaction" $ processTransactionSpec processTransaction
    describe "processTransaction'" $ processTransactionSpec processTransaction'
    describe "processTransactions" $ processTransactionsSpec processTransactions
    describe "processTransactions'" $ processTransactionsSpec processTransactions'

processTransactionSpec :: (Transaction -> UTXOs -> Either String UTXOs) -> SpecWith ()
processTransactionSpec p = do
    it "should work for a valid transaction with one input and one output" $ do
        let o  = Output 999 "Andres"
            tx = Transaction 1 [larsInput] [o]
        p tx genesis `shouldBe` Right (M.fromList [ (Input 1 0, Output 999 "Andres")
                                                  , (Input 0 1, Output 1000 "Andres")
                                                  ])
    it "should work for a valid transaction with two inputs and three outputs" $ do
        let o1 = Output 980 "Andres"
            o2 = Output 970 "Lars"
            o3 = Output  50 "Restaurant"
            tx = Transaction 1 [andresInput, larsInput] [o1, o2, o3]
        p tx genesis `shouldBe` Right (M.fromList [ (Input 1 0, Output 980 "Andres")
                                                  , (Input 1 1, Output 970 "Lars")
                                                  , (Input 1 2, Output  50 "Restaurant")
                                                  ])
    it "should fail for a transaction with unknown input" $ do
        let o   = Output 999 "Andres"
            i   = Input 666 1
            tx  = Transaction 1 [i] [o]
        p tx genesis `shouldBe` Left "non-existent input Input {iPrevious = 666, iIndex = 1} in transaction 1"
    it "should fail if the input value is too low" $ do
        let o  = Output 1001 "Andres"
            tx = Transaction 1 [larsInput] [o]
        p tx genesis `shouldBe` Left "input value 1000 less than output value 1001 in transaction 1"
    it "should fail if there are duplicate inputs" $ do
        let o  = Output 1 "Andres"
            tx = Transaction 1 [larsInput, larsInput] [o]
        p tx genesis `shouldBe` Left "non-existent input Input {iPrevious = 0, iIndex = 0} in transaction 1"

processTransactionsSpec :: ([Transaction] -> UTXOs -> Either String UTXOs) -> SpecWith ()
processTransactionsSpec p = do
    it "should work for valid transactions" $ do
        let o1  = Output 980 "Andres"
            o2  = Output 970 "Lars"
            o3  = Output  50 "Restaurant"
            tx1 = Transaction 1 [andresInput, larsInput] [o1, o2, o3]
            i1  = Input 1 2
            o4  = Output  10 "Waitress"
            o5  = Output  40 "Restaurant"
            tx2 = Transaction 2 [i1] [o4, o5]
        p [tx1, tx2] genesis `shouldBe` Right (M.fromList [ (Input 1 0, Output 980 "Andres")
                                                          , (Input 1 1, Output 970 "Lars")
                                                          , (Input 2 0, Output  10 "Waitress")
                                                          , (Input 2 1, Output  40 "Restaurant")
                                                          ])
    it "should fail for two transactions if the first one is invalid" $ do
        let o1  = Output 980 "Andres"
            o2  = Output 980 "Lars"
            o3  = Output  50 "Restaurant"
            tx1 = Transaction 1 [andresInput, larsInput] [o1, o2, o3]
            i1  = Input 1 2
            o4  = Output  10 "Waitress"
            o5  = Output  40 "Restaurant"
            tx2 = Transaction 2 [i1] [o4, o5]
        p [tx1, tx2] genesis `shouldBe` Left "input value 2000 less than output value 2010 in transaction 1"
    it "should fail for two transactions if the second one is invalid" $ do
        let o1  = Output 980 "Andres"
            o2  = Output 970 "Lars"
            o3  = Output  50 "Restaurant"
            tx1 = Transaction 1 [andresInput, larsInput] [o1, o2, o3]
            i1  = Input 1 2
            o4  = Output  20 "Waitress"
            o5  = Output  40 "Restaurant"
            tx2 = Transaction 2 [i1] [o4, o5]
        p [tx1, tx2] genesis `shouldBe` Left "input value 50 less than output value 60 in transaction 2"

larsInput :: Input
larsInput = Input 0 0

andresInput :: Input
andresInput = Input 0 1

larsOutput :: Output
larsOutput = Output 1000 "Lars"

andresOutput :: Output
andresOutput = Output 1000 "Andres"

genesis :: UTXOs
genesis = M.fromList [ (larsInput, larsOutput), (andresInput, andresOutput) ]

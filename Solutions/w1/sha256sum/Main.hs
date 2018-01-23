import Hashing            (sha256Sum)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= sha256Sum

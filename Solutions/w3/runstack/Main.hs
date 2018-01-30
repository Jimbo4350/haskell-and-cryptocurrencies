import Stack.Parse        (runProgramIO)
import System.Environment (getArgs)

main :: IO ()
main = do
    [file] <- getArgs
    runProgramIO file

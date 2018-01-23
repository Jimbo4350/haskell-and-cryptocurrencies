module Parser where

import Control.Applicative

newtype Parser t a = Parser {runParser :: [t] -> [(a, [t])]}

instance Functor (Parser t) where

    fmap f p = Parser $ \ts -> [(f a, ts') | (a, ts') <- runParser p ts]

instance Applicative (Parser t) where

    pure a = Parser $ \ts -> [(a, ts)]

    p <*> q = Parser $ \ts -> do
        (f, ts') <- runParser p ts
        (a, ts'') <- runParser q ts'
        return (f a, ts'')

instance Alternative (Parser t) where

    empty = Parser $ const []

    p <|> q = Parser $ \st -> runParser p st ++ runParser q st

eof :: Parser t ()
eof = Parser $ \ts -> case ts of
    [] -> [((),[])]
    _  -> []

satisfy :: (t -> Bool) -> Parser t t
satisfy p = Parser $ \ts -> case ts of
    []              -> []
    (t : ts)
        | p t       -> [(t, ts)]
        | otherwise -> []

token :: Eq t => t -> Parser t ()
token t = () <$ satisfy (== t)

test :: Show a => FilePath -> Parser Char a -> IO ()
test f p = do
    s <- readFile f
    mapM_ print (runParser p s)

newtype CSV = CSV [Line]
    deriving Show

newtype Line = Line [Cell]
    deriving Show

newtype Cell = Cell String
    deriving Show

parseCSV :: Parser Char CSV
parseCSV = CSV <$> many parseLine

parseLine :: Parser Char Line
parseLine = (\c cs -> Line (c : cs)) <$> parseCell <*> many (token ',' *> parseCell) <* token '\n'

parseCell :: Parser Char Cell
parseCell = Cell <$> ignoreWhitespace (many (satisfy (`notElem` ",\n ")))

whitespace :: Parser Char ()
whitespace = () <$ many (token ' ')

ignoreWhitespace :: Parser Char a -> Parser Char a
ignoreWhitespace p = (whitespace *> p) <* whitespace




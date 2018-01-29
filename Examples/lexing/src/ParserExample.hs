module ParserExample where

import Text.Parsec
import Text.Parsec.String

-- TRUE;FALSE;TRUE;FALSE
--

data MyToken = TRUE | FALSE | SEMICOLON -- The type of tokens we care about
    deriving (Show, Eq)

type Syntax = [MyToken] -- Let's say our language consists of TRUEs and FALSEs, separated by ';'

myToken :: Parser MyToken
myToken =     TRUE <$ string "TRUE"
          <|> FALSE <$ string "FALSE"
          <|> SEMICOLON <$ char ';'

myTokenAndPos :: Parser (MyToken, SourcePos)
myTokenAndPos = do
    pos <- getPosition
    t   <- myToken
    return (t, pos)

myTokens :: Parser [(MyToken, SourcePos)] -- this is our "lexer"
myTokens = many myTokenAndPos

type MyParser a = Parsec [(MyToken, SourcePos)] () a -- A parser type that parses lists of tokens instead of lists of characters

token' :: (MyToken -> Maybe a) -> MyParser a -- this is the key primitive to build higher-level parsers
token' f = token
    (show . fst)
    snd
    (f . fst)

token'' :: MyToken -> MyParser () -- this is like 'char' for characters
token'' t = token' $ \t' -> if t == t' then Just () else Nothing

trueOrFalse :: MyParser MyToken
trueOrFalse = (TRUE <$ token'' TRUE) <|> (FALSE <$ token'' FALSE)

syntax :: MyParser Syntax
syntax = sepBy trueOrFalse (token'' SEMICOLON)

parse' :: MyParser a -> SourceName -> String -> Either ParseError a -- combining lexing and parsing
parse' p n s = do
    ts <- parse myTokens n s
    parse p n ts

test1, test2, test3 :: Either ParseError Syntax
test1 = parse' (syntax <* eof) "" "FALSE;FALSE;TRUE;TRUE" -- succeeds
test2 = parse' (syntax <* eof) "" "FALSE;FALSE;THREE"     -- fails lexing
test3 = parse' (syntax <* eof) "" "FALSE;FALSE;TRUETRUE"  -- lexing succeeds, but parsing fails

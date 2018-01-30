{-# OPTIONS_HADDOCK ignore-exports #-}

{-|
Module      : Stack.Lex
Description : tokenizer for the Stack language
Copyright   : (c) Lars Bruenjes, 2017
License     : BSD3
Maintainer  : lars.bruenjes@iohk.io
Stability   : experimental
Portability : portable

This module defines an apropriate token type for the Stack
language and a parser to parse strings into lists of tokens.

Using the fact that Parsec supports arbitrary token types,
this means that the second phase of parsing
can work on lists of tokens instead of @`String`@s
and will not have to deal with things like
whitespace or tokens that start with the same letter
(like @push@ and @pop@).
-}

module Stack.Lex
    ( Token (..)
    , Tokens
    , SParser
    , lexStack
    , stack
    , push
    ) where

import Control.Monad       (void)
import Data.Maybe          (fromMaybe)
import Numeric.Natural     (Natural)
import Text.Parsec
import Text.Parsec.String

-- |The token type for the Stack language.
--
-- The only interesting thing is that I chose to represent
-- @push@ together with its @`Int`@ argument as
-- /one/ token. This makes whitespace handling simpler,
-- because I insist on one or more space-characters between
-- @push@ and its argument, but allow zero or more
-- space- and newline-characters between any two other tokens.
data Token =
      BLOCKSTART -- ^{
    | BLOCKEND   -- ^}
    | SEMICOLON  -- ^;
    | ADD        -- ^add
    | DUP        -- ^dup
    | MUL        -- ^mul
    | NEG        -- ^neg
    | OVER       -- ^over
    | POP        -- ^pop
    | PUSH Int   -- ^push n
    | SWAP       -- ^swap
    | HALT       -- ^halt
    | IFZERO     -- ^ifzero
    | LOOP       -- ^loop
    | RET        -- ^ret
    deriving (Show, Eq)

-- |The stream type for the next phase of parsing,
-- lists of position-token pairs.
--
-- The position is necessary for Parsec's error reporting
-- to work properly.
type Tokens = [(SourcePos, Token)]

-- |The parser type for the next phase of parsing:
-- It takes a stream of type @`Tokens`@ and has no
-- extra user state.
type SParser a = Parsec Tokens () a

-- |This functions performs the first phase of parsing,
-- lexing: It parses a @`String`@ into a @`Tokens`@ stream,
-- allowing arbitrary whitespace (space- and newline-characters)
-- between tokens.
--
-- Note that even though I do not require any whitspace between two
-- tokens, for the lexer, the grammar forbids this for Stack programs.
--
-- >>> map snd <$> parse lexStack "" " poppop  ; ; { swap  "
-- Right [POP,POP,SEMICOLON,SEMICOLON,BLOCKSTART,SWAP]
lexStack :: Parser Tokens
lexStack = ws *> sepEndBy ((,) <$> getPosition <*> stackToken) ws

-- |This is the fundamental primitive from which to define
-- @`SParser`@'s: Given a "predicate" @f :: `Token` -> `Maybe` a@,
-- @`stack'` f@ checks the next token and applies @f@ to it.
-- If the result is @`Just` a@, the parser succeeds and returns
-- @a@ as its result. Otherwise it fails /without consuming any input/.
--
-- By using @`token`@ internally,
-- it provides the link between tokens
-- and positions in the original @`String`@,
-- thus allowing error messages to point to
-- accurate source code positions.
stack' :: (Token -> Maybe a) -> SParser a
stack' f = token
    (show . snd)
    fst
    $ \(_, t) -> f t

-- |This is a speciallized version of @`stack'`@ which tries to parse
-- the specified token and does not return any result.
stack :: Token -> SParser ()
stack t =
        stack' (\t' -> if t' == t then Just () else Nothing)
    <?> show t

-- |A specialized parser for the @`PUSH`@ token which, if successful,
-- returns the @`Int`@-argument as result.
push :: SParser Int
push = stack' (\t -> case t of
    PUSH n -> Just n
    _      -> Nothing)
    <?> "PUSH"

-- |Parses one @`Token`@ from a @`String`@.
--
-- >>> parse stackToken "" "push   +17"
-- Right (PUSH 17)
-- >>> parse stackToken "" "pop"
-- Right POP
stackToken :: Parser Token
stackToken =
        BLOCKSTART <$ char '{'
    <|> BLOCKEND <$ char '}'
    <|> SEMICOLON <$ char ';'
    <|> ADD <$ string "add"
    <|> DUP <$ string "dup"
    <|> HALT <$ string "halt"
    <|> IFZERO <$ string "ifzero"
    <|> LOOP <$ string "loop"
    <|> MUL <$ string "mul"
    <|> NEG <$ string "neg"
    <|> OVER <$ string "over"

    -- POP and PUSH are the only two tokens that start with the same letter,
    -- so we have to left-factor and be careful!
    -- PUSH insists on one or more spaces between "push" and the Int
    -- argument.
    <|> (char 'p' *> (  POP <$ string "op"
                    <|> PUSH <$> (string "ush" *> many1 (char ' ') *> int)))

    <|> RET <$ string "ret"
    <|> SWAP <$ string "swap"

-- |Parses a sign (@+@ or @-@) and returns the associated operation on numbers
-- (@`id`@ for @+@, @`negate`@ for @-@).
sign :: Num a => Parser (a -> a)
sign = id <$ char '+' <|> negate <$ char '-'

-- |Parses a non-zero digit.
--
-- >>> parse nzdigit "" "1"
-- Right '1'
-- >>> parse nzdigit "" "0"
-- Left (line 1, column 1):
-- unexpected "0"
-- expecting non-zero digit
nzdigit :: Parser Char
nzdigit = oneOf "123456789" <?> "non-zero digit"

-- |Parses a natural number.
--
-- >>> parse nat "" "123"
-- Right 123
nat :: Parser Natural
nat =  (0 <$ char '0' <|> (\d ds -> read (d: ds)) <$> nzdigit <*> many digit)
    <?> "natural number"

-- |Parses an @`Integer`@, allowing an optional sign as first character.
--
-- >>> parse integer "" "-999888777666555444333222111"
-- Right (-999888777666555444333222111)
integer :: Parser Integer
integer = (fromMaybe id <$> optionMaybe sign <*> nat') <?> "Integer"
  where
    nat' :: Parser Integer
    nat' = fromIntegral <$> nat

-- |Tries to convert an @`Integer`@ into an @`Int`@ of the same value;
-- returns @`Nothing`@ if the absolute value is too large.
--
-- >>> integerToInt (-42)
-- Just (-42)
-- >>> integerToInt (-999888777666555444333222111)
-- Nothing
integerToInt :: Integer -> Maybe Int
integerToInt n =
    let minb = fromIntegral (minBound :: Int)
        maxb = fromIntegral (maxBound :: Int)
    in  if n >= minb && n <= maxb
            then Just $ fromIntegral n
            else Nothing

-- |Parses an @`Int`@, allowing an optional sign as first character.
--
-- >>> parse int "" "+666"
-- Right 666
-- >>> parse int "" "-999888777666555444333222111"
-- Left (line 1, column 29):
-- unexpected end of input
-- expecting digit
-- out of range
--
-- Note that the error message is not perfect,
-- but at least it says "out of range".
int :: Parser Int
int = (do
    n <- integer
    case integerToInt n of
        Nothing -> parserFail "out of range"
        Just m  -> return m)
    <?> "Int"

-- |Parses whitespace, i.e. zero or more occurences
-- of @' '@ or @'\n'@.
--
-- Note that this parser /can not fail/!
ws :: Parser ()
ws = void $ many $ oneOf " \n"

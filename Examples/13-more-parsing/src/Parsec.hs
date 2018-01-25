module Parsec where

import           Control.Monad      (void)
import           Text.Parsec
import           Text.Parsec.String

type CSV = [[String]]

ws :: Parser ()
ws = void $ many (char ' ')

cell :: Parser String
cell = strip <$> many (noneOf ",\n")

line :: Parser [String]
line = sepBy cell (ws *> char ',' *> ws)

csv :: Parser CSV
csv = many (line <* char '\n')

stripStart :: String -> String
stripStart ""        = ""
stripStart (' ' : s) = stripStart s
stripStart s         = s

stripEnd :: String -> String
stripEnd = reverse . stripStart . reverse

strip :: String -> String
strip = stripEnd . stripStart

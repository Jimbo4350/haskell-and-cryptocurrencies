module Parser where

import Control.Applicative
import Control.Monad
import Data.List (foldl')

newtype Parser t a = Parser {runParser :: [t] -> [(a, [t])]}

instance Functor (Parser t) where

    fmap = liftM

instance Applicative (Parser t) where

    pure = return

    (<*>) = ap

instance Monad (Parser t) where

    return a = Parser $ \ts -> [(a, ts)]

    p >>= cont = Parser $ \ts -> do
        (a, ts') <- runParser p ts
        runParser (cont a) ts'

instance Alternative (Parser t) where

    empty = Parser $ const []

    p <|> q = Parser $ \st -> runParser p st ++ runParser q st

instance MonadPlus (Parser t) where

    mzero = empty

    mplus = (<|>)

eof :: Parser t ()
eof = Parser $ \ts -> case ts of
    [] -> [((),[])]
    _  -> []

satisfy :: (t -> Bool) -> Parser t t
satisfy p = Parser $ \ts -> case ts of
    []              -> []
    (x : xs)
        | p x       -> [(x, xs)]
        | otherwise -> []

token :: Eq t => t -> Parser t ()
token t = () <$ satisfy (== t)

listOf :: Parser t a -> Parser t b -> Parser t [a]
listOf p sep = (:) <$> p <*> many (sep *> p)

chainl :: Parser t a -> Parser t (a -> a -> a) -> Parser t a
chainl p op =     foldl' (flip ($))
              <$> p
              <*> many (flip <$> op <*> p)

pythagoras :: Int -> [(Int, Int, Int)]
pythagoras n = do
    x <- [1..n]
    y <- [1..n]
    z <- [1..n]
    guard (x*x+y*y==z*z)
    return (x,y,z)

-- nat
--
-- pal -> epsilon
-- pal -> nat
-- pal -> nat pal nat

digit, digitNZ :: Parser Char Char
digit   = satisfy (`elem` "0123456789")
digitNZ = satisfy (`elem` "123456789")

nat :: Parser Char Int
nat =     (0                        <$ token '0')
      <|> ((\d ds -> read (d : ds)) <$> digitNZ <*> many digit)

pal :: Parser Char [Int]
pal =     pure []
      <|> (: []) <$> nat
      <|> p
  where
    p = do
        n <- nat
        token ' '
        ns <- pal
        void $ optional (token ' ')
        n' <- nat
        guard (n == n')
        return $ n : ns ++ [n']

data S = S D (Maybe Z) deriving Show -- Digit D | Minus D Z -- Minus S D | Digit D

data Z = Z D (Maybe Z) deriving Show -- Digit' D | Minus' D Z

data D = Zero | One deriving Show

parseS :: Parser Char S
parseS = S <$> parseD <*> optional parseZ

parseZ :: Parser Char Z
parseZ = Z <$> (token '-' *> parseD) <*> optional parseZ

parseD :: Parser Char D
parseD = Zero <$ token '0' <|> One <$ token '1'

pm :: Parser Char (Int -> Int -> Int)
pm = (+) <$ token '+' <|> (-) <$ token '-'

data E = Plus E E | Minus E E | Times E E | Lit Int deriving Show

e1, e2, e3 :: Parser Char E
e1 = chainl e2 op1
e2 = chainl e3 op2
e3 = (token '(' *> e1 <* token ')') <|> Lit <$> nat

op1, op2 :: Parser Char (E -> E -> E)
op1 = Plus <$ token '+' <|> Minus <$ token '-'
op2 = Times <$ token '*'

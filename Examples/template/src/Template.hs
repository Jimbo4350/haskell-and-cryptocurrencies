{-# LANGUAGE TemplateHaskell #-}

module Template
    ( mkProject
    , mkPrimes
    , ternary
    ) where

import Control.Monad             (replicateM)
import Data.List                 (foldl')
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Read                 (readMaybe)

mkProject :: Int -> Int -> DecsQ
mkProject m n
    | m <= 0         = fail "projectE m n: m must be positive"
    | n < 1 || n > m = fail "projectE m n: n must be in [1..m]"
    | otherwise      = do
        as <- replicateM m $ VarT <$> newName "a"
        let an  = as !! pred n
            t   = foldl' AppT (TupleT m) as
            sig = AppT (AppT ArrowT t) an
        x  <- newName "x"
        let f = mkName $ "project_" ++ show m ++ "_" ++ show n
            p = TupP [if i /= n then WildP else VarP x | i <- [1..m]]
        return [ SigD f sig
               , FunD f [Clause [p] (NormalB $ VarE x) []]
               ]

isPrime :: Int -> Bool
isPrime p = p >= 2 && notElem 0 [p `mod` x | x <- [2 .. pred p]]

primes :: Int -> Int -> [Int]
primes a b = filter isPrime [a..b]

mkPrimes :: Int -> Int -> ExpQ
mkPrimes a b = do
    let ps = primes a b
    [| ps |]

parseTernary :: String -> Q Integer
parseTernary s = case readMaybe s of
    Nothing -> e
    Just n  -> (* signum n) <$> go (abs n)
  where
    e = fail $ "Cannot parse '" ++ s ++ "' as a ternary literal."

    go :: Integer -> Q Integer
    go 0 = return 0
    go n = case n `mod` 10 of
        d | d <= 2    -> (\x -> 3 * x + d) <$> go (n `div` 10)
          | otherwise -> e

ternaryExp :: String -> ExpQ
ternaryExp s = do
    n <- parseTernary s
    [| n |]

ternaryPat :: String -> PatQ
ternaryPat s = (LitP . IntegerL) <$> parseTernary s

ternary :: QuasiQuoter
ternary = QuasiQuoter
    { quoteExp  = ternaryExp
    , quotePat  = ternaryPat
    , quoteType = const $ fail "The 'ternary' quasi quoter cannot be used in a type context."
    , quoteDec  = const $ fail "The 'ternary' quasi quoter cannot be used in a declaration context."
    }

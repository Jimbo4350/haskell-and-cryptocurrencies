{-# OPTIONS_HADDOCK ignore-exports #-}

{-|
Module      : Stack.Parse
Description : parser for the Stack language
Copyright   : (c) Lars Bruenjes, 2017
License     : BSD3
Maintainer  : lars.bruenjes@iohk.io
Stability   : experimental
Portability : portable

This module enables parsing a list of tokens
from the "Stack.Lex" module
into a Stack program.
-}

module Stack.Parse
    ( parseProgram
    , parseProgramIO
    , runProgramIO
    ) where

import Stack.Core
import Stack.Lex
import Text.Parsec

-- |Parses a stack program.
--
-- >>> run <$> parseProgram "" "{push +17; dup; halt}"
-- Right (Just [17,17])
parseProgram :: SourceName -- ^source file name (can be "")
             -> String     -- ^source code
             -> Either ParseError Instructions
parseProgram n s = do
    b <- parse' block n s
    return $ blockToIns Halt b

-- |Parses a Stack program from a file.
parseProgramIO :: FilePath -> IO (Either ParseError Instructions)
parseProgramIO file = do
    s <- readFile file
    return $ parseProgram file s

-- |Parses and runs a Stack program from a file.
runProgramIO :: FilePath -> IO ()
runProgramIO file = do
    ep <- parseProgramIO file
    case ep of
        Left e  -> print e
        Right p -> case run p of
            Nothing -> putStrLn "stack exhausted!"
            Just xs -> print xs

-- |First tokenizes a @`String`@,
-- then applies an @`SParser`@.
parse' :: SParser a  -- ^@`Token`@-based parser to apply after lexing
       -> SourceName -- ^source file name (can be empty)
       -> String     -- ^source code
       -> Either ParseError a
parse' p n s = parse lexStack n s >>= parse p n

-- |Type corresponding to the @block@ nonterminal.
newtype Block = Block Instrs deriving Show

-- |Type corresponding to the @instrs@ nonterminal.
data Instrs =
      Simple Simple Instrs
    | Ctrl Ctrl
    deriving Show

-- |Type corresponding to the @simple@ nonterminal.
data Simple = SPush Int | SAdd | SMul | SDup | SSwap | SNeg | SPop | SOver
    deriving Show

-- |Type corresponding to the @ctrl@ nonterminal.
data Ctrl =
      CIfZero Block Block
    | CLoop Block
    | CHalt
    | CRet
    deriving Show

-- |Semantic function to interpret a block as a Stack program.
--
-- >>> run (blockToIns Halt $ Block $ Ctrl CHalt)
-- Just []
blockToIns :: Instructions -- ^the active "return address": @`Halt`@ outside of any loop; otherwise the loop start
           -> Block        -- ^the block to interpret
           -> Instructions
blockToIns ret (Block ins) = instrsToIns ret ins

-- |Semantic function to interpret @`Instrs`@ as a Stack program.
--
-- >>> run (instrsToIns Halt (Ctrl CRet))
-- Just []
instrsToIns :: Instructions -- ^the active "return address": @`Halt`@ outside of any loop; otherwise the loop start
            -> Instrs       -- ^the @`Instrs`@ to interpret
            -> Instructions
instrsToIns ret (Simple s ins) = simpleToIns s $ instrsToIns ret ins
instrsToIns ret (Ctrl c)    = ctrlToIns ret c

-- |Semantic function to interpret @`Simple`@ as a Stack program.
--
-- >>> run' (simpleToIns SOver Halt) [1, 2]
-- Just [2,1,2]
simpleToIns :: Simple       -- ^the @`Simple`@ to interpret
            -> Instructions -- ^continuation
            -> Instructions
simpleToIns (SPush n) = Push n
simpleToIns SAdd      = Add
simpleToIns SMul      = Mul
simpleToIns SDup      = Dup
simpleToIns SSwap     = Swap
simpleToIns SNeg      = Neg
simpleToIns SPop      = Pop
simpleToIns SOver     = Over

-- |Semantic function to interpret @`Ctrl`@ as a Stack program.
--
-- >>> run (ctrlToIns Halt CHalt)
-- Just []
ctrlToIns :: Instructions -- ^the active "return address": @`Halt`@ outside of any loop; otherwise the loop start
          -> Ctrl         -- ^the @`ctrl`@ to interpret
          -> Instructions
ctrlToIns ret (CIfZero b b') = IfZero (blockToIns ret b) (blockToIns ret b')
ctrlToIns _   (CLoop b)      = Loop $ \loop -> blockToIns loop b
ctrlToIns _   CHalt          = Halt
ctrlToIns ret CRet           = ret

-- |Parses a @`Block`@.
--
-- >>> parse' block "" "{ halt }"
-- Right (Block (Ctrl CHalt))
block :: SParser Block
block = Block <$> (stack BLOCKSTART *> instrs <* stack BLOCKEND)

-- |Parses @`Instrs`@.
--
-- >>> parse' instrs "" "add; ret"
-- Right (Simple SAdd (Ctrl CRet))
instrs :: SParser Instrs
instrs =
        Simple <$> simple <*> (stack SEMICOLON *> instrs)
    <|> Ctrl   <$> ctrl

-- |Parses @`Simple`@.
--
-- >>> parse' simple "" "push 23"
-- Right (SPush 23)
simple :: SParser Simple
simple =
        SPush <$> push
    <|> SAdd  <$  stack ADD
    <|> SMul  <$  stack MUL
    <|> SDup  <$  stack DUP
    <|> SSwap <$  stack SWAP
    <|> SNeg  <$  stack NEG
    <|> SPop  <$  stack POP
    <|> SOver <$  stack OVER

-- |Parses @`Ctrl`@.
--
-- >>> parse' ctrl "" "halt"
-- Right CHalt
ctrl :: SParser Ctrl
ctrl =
        CIfZero <$> (stack IFZERO *> block) <*> block
    <|> CLoop   <$> (stack LOOP *> block)
    <|> CHalt   <$  stack HALT
    <|> CRet    <$  stack RET

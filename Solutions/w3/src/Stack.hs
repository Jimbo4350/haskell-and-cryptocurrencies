{-# OPTIONS_HADDOCK ignore-exports #-}

{-|
Module      : Stack
Description : Stack language
Copyright   : (c) Lars Bruenjes, 2017
License     : BSD3
Maintainer  : lars.bruenjes@iohk.io
Stability   : experimental
Portability : portable

Provides functionality for parsing and executing programs written in the Stack language.
-}

module Stack
    ( module Stack.Core
    , module Stack.Lex
    , module Stack.Parse
    , fact
    , fact5
    ) where

import Stack.Core
import Stack.Lex
import Stack.Parse

fact :: Instructions
fact =
  Push 1 $
  Swap $
  Loop $ \ loop ->
  Dup $
  IfZero (Pop Halt) $
  Swap $
  Over $
  Mul $
  Swap $
  Push 1 $
  Neg $
  Add
  loop

fact5 :: Instructions
fact5 = Push 5 fact

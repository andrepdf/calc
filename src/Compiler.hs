{-# LANGUAGE CPP #-}

module Compiler
    ( compile )
where

#include "Bytecode.h"

import Data.Word ( Word8 )
import Data.Bits ( shiftR )
import GHC.Float ( castDoubleToWord64 )
import Syntax    ( Expr(..) )

compile :: Expr -> [Word8]
compile expr = comp expr ++ [EXIT]

comp :: Expr -> [Word8]
comp (Val x)   = PUSH : doubleToBytes x
comp (Add x y) = comp x ++ comp y ++ [ADD]
comp (Sub x y) = comp x ++ comp y ++ [SUB]
comp (Mul x y) = comp x ++ comp y ++ [MUL]
comp (Div x y) = comp x ++ comp y ++ [DIV]

doubleToBytes :: Double -> [Word8]
doubleToBytes d =
    let w = castDoubleToWord64 d in
    [ fromIntegral $ w `shiftR` n | i <- [0..7], let n = i * 8 ]

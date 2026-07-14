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
compile expr = compile' expr ++ [EXIT]

compile' :: Expr -> [Word8]
compile' (Val x)   = PUSH : doubleToBytes x
compile' (Add x y) = compile' x ++ compile' y ++ [ADD]
compile' (Sub x y) = compile' x ++ compile' y ++ [SUB]
compile' (Mul x y) = compile' x ++ compile' y ++ [MUL]
compile' (Div x y) = compile' x ++ compile' y ++ [DIV]

doubleToBytes :: Double -> [Word8]
doubleToBytes d =
    let w = castDoubleToWord64 d in
    [ fromIntegral $ w `shiftR` n | i <- [0..7], let n = i * 8 ]

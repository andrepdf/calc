{-# LANGUAGE CPP #-}

module Compiler
    ( compile )
where

#include "Bytecode.h"

import Data.Word        ( Word8 )
import Foreign.C.Types  ( CSize(..) )
import Foreign.Storable ( Storable, sizeOf )
import Data.Bits        ( Bits, shiftR )
import GHC.Float        ( castDoubleToWord64 )

import Syntax ( Expr(..) )

type Env = [(String, CSize)]

compile :: Expr -> [Word8]
compile expr = comp [] expr ++ [EXIT]

comp :: Env -> Expr -> [Word8]
comp _   (Val n)     = PUSH : toBytes (castDoubleToWord64 n)
comp env (Add a b)   = comp env a ++ comp (inc env) b ++ [ADD]
comp env (Sub a b)   = comp env a ++ comp (inc env) b ++ [SUB]
comp env (Mul a b)   = comp env a ++ comp (inc env) b ++ [MUL]
comp env (Div a b)   = comp env a ++ comp (inc env) b ++ [DIV]
comp env (Var x)     =
    case lookup x env of
        Nothing -> error $ "undefined variable '" ++ x ++ "'" -- TODO: handle error
        Just o  -> GET : toBytes o
comp env (Let x a b) = comp env a ++ comp ((x, 1) : inc env) b

inc :: Env -> Env
inc = map (\(x, o) -> (x, o + 1))

toBytes :: (Storable a, Bits a, Integral a) => a -> [Word8]
toBytes s =
    let n = sizeOf s in
    [ fromIntegral $ s `shiftR` (i * 8) | i <- [0..(n - 1)] ]

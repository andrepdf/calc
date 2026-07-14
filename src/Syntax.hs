module Syntax where

data Expr
    = Val Double
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    deriving Show

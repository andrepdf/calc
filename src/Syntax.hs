module Syntax
    ( Expr(..) )
where

data Expr
    = Val Double
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Var String
    | Let String Expr Expr
    deriving Show

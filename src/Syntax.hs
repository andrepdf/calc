module Syntax
    ( Expr(..) )
where

import Test.QuickCheck

data Expr
    = Val Double
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Var String
    | Let String Expr Expr
    deriving Show

instance Arbitrary Expr where
    -- arbitrary :: Gen Expr
    arbitrary = arbitraryExpr 5 []

arbitraryExpr :: Int -> [String] -> Gen Expr
arbitraryExpr 0 [] = Val <$> arbitrary
arbitraryExpr 0 env = oneof
    [ Val <$> arbitrary
    , Var <$> elements env ]
arbitraryExpr i env =
    let j = i - 1 in oneof
    [ Add <$> arbitraryExpr j env <*> arbitraryExpr j env
    , Sub <$> arbitraryExpr j env <*> arbitraryExpr j env
    , Mul <$> arbitraryExpr j env <*> arbitraryExpr j env
    , Div <$> arbitraryExpr j env <*> arbitraryExpr j env
    , do
        let x = 'x' : show (length env)
        a <- arbitraryExpr j env
        b <- arbitraryExpr j (x : env)
        pure $ Let x a b ]

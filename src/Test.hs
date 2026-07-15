module Test
    ( test )
where

import Control.Monad.IO.Class ( liftIO )
import Test.QuickCheck

import Syntax      ( Expr(..) )
import Compiler    ( compile )
import Interpreter ( interpret )

test :: IO ()
test = do
    quickCheck prop_validInterpreter

prop_validInterpreter :: Expr -> Property
prop_validInterpreter expr = ioProperty $ do
    let res1 = refInterpret [] expr
    x <- interpret $ compile expr
    pure $ case x of
        Nothing   -> False
        Just res2 -> compile (Val res1) == compile (Val res2)

refInterpret :: [(String, Double)] -> Expr -> Double
refInterpret _   (Val n)     = n
refInterpret env (Add a b)   = refInterpret env a + refInterpret env b
refInterpret env (Sub a b)   = refInterpret env a - refInterpret env b
refInterpret env (Mul a b)   = refInterpret env a * refInterpret env b
refInterpret env (Div a b)   = refInterpret env a / refInterpret env b
refInterpret env (Var x)     =
    case lookup x env of
        Nothing -> error $ "undefined variable '" ++ x ++ "'" -- TODO: handle error
        Just a  -> a
refInterpret env (Let x a b) =
    let n = refInterpret env a in
    let nenv = (x, n) : env in
    refInterpret nenv b

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

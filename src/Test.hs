module Test
    ( test )
where

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

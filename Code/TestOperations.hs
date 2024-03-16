{-# LANGUAGE TemplateHaskell #-}
module TestOperations where

import Test.QuickCheck

import Expr
import Test

-- Constructor for an expression that takes two arguments such as Add / Sub
type TwoArgOperation = Expr -> Expr -> Expr

-- An operation that takes two integers, such as (+) or (-)
type TwoIntOperation = Int -> Int -> Int

{-
 - Helper function to verify that a given Expr that takes two int arguments
 - always suceeds when given two ints and produces the same result as the
 - simple operation
-}
testEval2IntArithmetic :: TwoArgOperation -> TwoIntOperation -> Int -> Int -> Bool
testEval2IntArithmetic opExpr fResult x y = case eval [] expr of
        Nothing -> False
        Just v -> v == expected
    where
        expr = opExpr (Val (IntVal x)) (Val (IntVal y))
        expected = IntVal (fResult x y)

prop_testEvalAdd :: Int -> Int -> Bool
prop_testEvalAdd = testEval2IntArithmetic Add (+)

prop_testEvalSub :: Int -> Int -> Bool
prop_testEvalSub = testEval2IntArithmetic Sub (\x y -> x - y)

prop_testEvalMul :: Int -> Int -> Bool
prop_testEvalMul = testEval2IntArithmetic Mul (*)

-- TODO: Make tests for divisor == 0

zero_i_expr :: Expr
zero_i_expr = Val (IntVal 0)
zero_f_expr :: Expr
zero_f_expr = Val (FloatVal 0.0)

-- Test integer division by first multiplying to ensure that it will divide
prop_testIntDiv :: Int -> Int -> Property
prop_testIntDiv a b = a /= 0 ==> testEval2IntArithmetic Div (\_ _ -> b) ab a
    where ab = a * b

prop_testEvalDivInt0 :: NumberValue -> Bool
prop_testEvalDivInt0 (NumberValue v) = eval [] expr == Nothing
    where expr = Div (Val v) zero_i_expr

prop_testEvalDivFloat0 :: NumberValue -> Bool
prop_testEvalDivFloat0 (NumberValue v) = eval [] expr == Nothing
    where expr = Div (Val v) zero_f_expr

-- MOD
prop_testEvalMod :: Int -> Int -> Property
prop_testEvalMod a b = b /= 0 ==> testEval2IntArithmetic Mod mod a b

prop_testEvalModInt0 :: NumberValue -> Bool
prop_testEvalModInt0 (NumberValue v) = eval [] expr == Nothing
    where expr = Mod (Val v) zero_i_expr

prop_testEvalModFloat0 :: NumberValue -> Bool
prop_testEvalModFloat0 (NumberValue v) = eval [] expr == Nothing
    where expr = Mod (Val v) zero_f_expr

-- POW
prop_testEvalPowIntNon0 :: Int -> Int -> Property
prop_testEvalPowIntNon0 a x = a /= 0 ==> testEval2IntArithmetic Pow (^) a x

-- Skip sqrt-like as complex for negative a
prop_testEvalPowFloatReg :: Positive Float -> Float -> Property
prop_testEvalPowFloatReg (Positive a) x = a /= 0 ==> eval [] expr === Just expected
    where
        expr = Pow (Val (FloatVal a)) (Val (FloatVal x))
        expected = FloatVal (a**x)

prop_testEvalPowFloatFractional :: Negative Float -> Float -> Property
prop_testEvalPowFloatFractional (Negative a) x = isNaN (a**x) ==> eval [] expr === Nothing
    where
        expr = Pow (Val (FloatVal a)) (Val (FloatVal x))

-- 0^x positive
prop_testEvalPow0IntPos :: Positive Int -> Property
prop_testEvalPow0IntPos (Positive x) = eval [] expr === Just (IntVal 0)
    where expr = Pow zero_i_expr (Val (IntVal x))

prop_testEvalPow0FloatPos :: Positive Float -> Property
prop_testEvalPow0FloatPos (Positive x) = eval [] expr === Just (FloatVal 0.0)
    where expr = Pow zero_f_expr (Val (FloatVal x))

-- 0^ negative is like divide by zero
prop_testEvalPowInt0Neg :: Negative Int -> Property
prop_testEvalPowInt0Neg (Negative negInt) = eval [] expr === Nothing
    where expr = Pow zero_i_expr (Val (IntVal negInt))

prop_testEvalPowFloat0Neg :: Negative Float -> Property
prop_testEvalPowFloat0Neg (Negative negFloat) = eval [] expr === Nothing
    where expr = Pow zero_f_expr (Val (FloatVal negFloat))


-- ABS (INTEGERS)
prop_testEvalAbsNegInt :: Negative Int -> Bool
prop_testEvalAbsNegInt (Negative i) = eval [] expr == Just expected
    where
        expr = (Abs . Val . IntVal) i
        expected = IntVal (-i)

prop_testEvalAbsPosInt :: Positive Int -> Bool
prop_testEvalAbsPosInt (Positive i) = eval [] expr == Just expected
    where
        expr = (Abs . Val . IntVal) i
        expected = IntVal i

-- ABS (FLOATS)
prop_testEvalAbsNegFloat :: Negative Float -> Bool
prop_testEvalAbsNegFloat (Negative f) = eval [] expr == Just expected
    where
        expr = (Abs . Val . FloatVal) f
        expected = FloatVal (-f)

prop_testEvalAbsPosFloat :: Positive Float -> Bool
prop_testEvalAbsPosFloat (Positive f) = eval [] expr == Just expected
    where
        expr = (Abs . Val . FloatVal) f
        expected = FloatVal f

return []
runOperationTests = $quickCheckAll

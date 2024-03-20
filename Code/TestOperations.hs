{-# LANGUAGE TemplateHaskell #-}
module TestOperations where

import Test.QuickCheck

import Expr
import Eval
import BinaryTree

import Test

newtype NumberValue = NumberValue Value
    deriving (Show)

instance Arbitrary NumberValue where
    arbitrary = fmap NumberValue (oneof [
        fmap (IntVal) arbitrary,
        fmap (FloatVal) arbitrary
        ])


-- Constructor for an expression that takes two arguments such as Add / Sub
type TwoArgOperation = Expr -> Expr -> Expr

-- An operation that takes two integers, such as (+) or (-)
type TwoIntOperation = Int -> Int -> Int
-- Same but for Float
type TwoFloatOperation = Float -> Float -> Float
type FloatAndIntOperation = Float -> Int -> Float

{-
 - Helper function to verify that a given Expr that takes two int arguments
 - always suceeds when given two ints and produces the same result as the
 - simple operation
-}
testEval2IntArithmetic :: TwoArgOperation -> TwoIntOperation -> Int -> Int -> Bool
testEval2IntArithmetic opExpr fResult x y = evalBasic expr == Right expected
    where
        expr = opExpr (Val (IntVal x)) (Val (IntVal y))
        expected = IntVal (fResult x y)

testEval2FloatArithmetic :: TwoArgOperation -> TwoFloatOperation -> Float -> Float -> Bool
testEval2FloatArithmetic opExpr fResult x y = evalBasic expr == Right expected
    where
        expr = opExpr (Val (FloatVal x)) (Val (FloatVal y))
        expected = FloatVal (fResult x y)

testEval2MixedComArithmetic :: TwoArgOperation -> (Float -> Float -> Float) -> Int -> Float -> Property
testEval2MixedComArithmetic opExpr fResult i f = evalBasic expr1 === Right expected .&&. evalBasic expr2 === Right expected
    where
        a = Val (IntVal i)
        b = Val (FloatVal f)
        expr1 = opExpr a b
        expr2 = opExpr b a
        expected = FloatVal (fResult f (fromIntegral i))

{- Addition a + b -}
prop_testEvalAddInts :: Int -> Int -> Bool
prop_testEvalAddInts = testEval2IntArithmetic Add (+)

prop_testEvalAddFloats :: Float -> Float -> Bool
prop_testEvalAddFloats = testEval2FloatArithmetic Add (+)

prop_testEvalAddMixed :: Int -> Float -> Property
prop_testEvalAddMixed = testEval2MixedComArithmetic Add (+)

{- Subtraction a - b -}
prop_testEvalSubInts :: Int -> Int -> Bool
prop_testEvalSubInts = testEval2IntArithmetic Sub (-)

prop_testEvalSubFloats :: Float -> Float -> Bool
prop_testEvalSubFloats = testEval2FloatArithmetic Sub (-)

prop_testEvalSubIntFloat :: Int -> Float -> Property
prop_testEvalSubIntFloat i f = evalBasic expr === Right expected
    where
        expr = Sub (Val (IntVal i)) (Val (FloatVal f))
        expected = FloatVal $ (fromIntegral i) - f

prop_testEvalSubFloatInt :: Float -> Int -> Property
prop_testEvalSubFloatInt f i = evalBasic expr === Right expected
    where
        expr = Sub (Val (IntVal i)) (Val (FloatVal f))
        expected = FloatVal $ (fromIntegral i) - f

{- Multiplication a * b -}
prop_testEvalMulInts :: Int -> Int -> Bool
prop_testEvalMulInts = testEval2IntArithmetic Mul (*)

prop_testEvalMulFloats :: Float -> Float -> Bool
prop_testEvalMulFloats = testEval2FloatArithmetic Mul (*)

prop_testEvalMulMixed :: Int -> Float -> Property
prop_testEvalMulMixed = testEval2MixedComArithmetic Mul (*)



zero_i_expr :: Expr
zero_i_expr = Val (IntVal 0)
zero_f_expr :: Expr
zero_f_expr = Val (FloatVal 0.0)

ensureMathError :: Show a => Either Error a -> Property
ensureMathError (Left (MathError _)) = property True
ensureMathError other = counterexample ("Expected MathError, got: " ++ show other) False

{- Division a / b -}
-- Test integer division by first multiplying to ensure that it will divide
prop_testIntDiv :: Int -> Int -> Property
prop_testIntDiv a b = a /= 0 ==> testEval2IntArithmetic Div (\_ _ -> b) ab a
    where ab = a * b

prop_testEvalDiv0 :: NumberValue -> Property
prop_testEvalDiv0 (NumberValue v) = ensureMathError $ evalBasic expr
    where expr = Div (Val v) zero_i_expr


{- Modulo Tests -}
prop_testEvalMod :: Int -> Int -> Property
prop_testEvalMod a b = b /= 0 ==> testEval2IntArithmetic Mod mod a b

prop_testEvalMod0 :: NumberValue -> Property
prop_testEvalMod0 (NumberValue v) = ensureMathError $ evalBasic expr
    where expr = Mod (Val v) zero_i_expr


{- Power AKA a^x tests -}
prop_testEvalPowIntNon0 :: Int -> Int -> Property
prop_testEvalPowIntNon0 a x = a /= 0 ==> testEval2IntArithmetic Pow (^) a x

-- Skip sqrt-like as complex for negative a
prop_testEvalPowFloatReg :: Positive Float -> Float -> Property
prop_testEvalPowFloatReg (Positive a) x = a /= 0 ==> evalBasic expr === Right expected
    where
        expr = Pow (Val (FloatVal a)) (Val (FloatVal x))
        expected = FloatVal (a**x)

prop_testEvalPowFloatFractional :: Negative Float -> Float -> Property
prop_testEvalPowFloatFractional (Negative a) x = isNaN (a**x) ==> ensureMathError $ evalBasic expr
    where
        expr = Pow (Val (FloatVal a)) (Val (FloatVal x))

-- 0^x positive
prop_testEvalPow0IntPos :: Positive Int -> Property
prop_testEvalPow0IntPos (Positive x) = evalBasic expr === Right (IntVal 0)
    where expr = Pow zero_i_expr (Val (IntVal x))

prop_testEvalPow0FloatPos :: Positive Float -> Property
prop_testEvalPow0FloatPos (Positive x) = evalBasic expr === Right (FloatVal 0.0)
    where expr = Pow zero_f_expr (Val (FloatVal x))

-- 0^ negative is like divide by zero
prop_testEvalPowInt0Neg :: Negative Int -> Property
prop_testEvalPowInt0Neg (Negative negInt) = ensureMathError $ evalBasic expr
    where expr = Pow zero_i_expr (Val (IntVal negInt))

prop_testEvalPowFloat0Neg :: Negative Float -> Property
prop_testEvalPowFloat0Neg (Negative negFloat) = ensureMathError $ evalBasic expr
    where expr = Pow zero_f_expr (Val (FloatVal negFloat))


{- Abs(x) - Absolute value -}
-- ABS (INTEGERS)

isVNonNeg :: Value -> Bool
isVNonNeg v = case v of
                    FloatVal f -> f >= 0
                    IntVal i -> i >= 0

prop_testEvalPosNum :: NumberValue -> Property
prop_testEvalPosNum (NumberValue v) = isVNonNeg v ==> evalBasic expr === Right v
    where expr = Abs (Val v)

prop_testEvalAbsNegInt :: Negative Int -> Bool
prop_testEvalAbsNegInt (Negative i) = evalBasic expr == Right expected
    where
        expr = (Abs . Val . IntVal) i
        expected = IntVal (-i)

prop_testEvalAbsNegFloat :: Negative Float -> Bool
prop_testEvalAbsNegFloat (Negative f) = evalBasic expr == Right expected
    where
        expr = (Abs . Val . FloatVal) f
        expected = FloatVal (-f)

{- Square Root sqrt() -}
prop_testSqrtInt :: Positive Int -> Property
prop_testSqrtInt (Positive x) = evalBasic expr === Right expected
    where
        expr = Sqrt (Val (IntVal (x*x)))
        expected = FloatVal (fromIntegral x)

prop_testSqrtNegInt :: Negative Int -> Property
prop_testSqrtNegInt (Negative x) = ensureMathError $ evalBasic expr
    where expr = Sqrt (Val (IntVal x))

prop_testSqrtFloat :: Positive Float -> Property
prop_testSqrtFloat (Positive x) = evalBasic expr === Right expected
    where
        expr = Sqrt (Val (FloatVal (x*x)))
        expected = FloatVal x

prop_testSqrtNegFloat :: Negative Float -> Property
prop_testSqrtNegFloat (Negative x) = ensureMathError $ evalBasic expr
    where expr = Sqrt (Val (FloatVal x))


{- Call Functions -}

-- Zero arg function that returns a "constant"
prop_testZeroArgFunc :: Value -> Property
prop_testZeroArgFunc v = eval Empty funcs (CallUserFunc "f" []) === Right v
    where
        func = UserFunc [] [] (Val v)
        funcs = insert ("f", func) Empty

-- Single arg function that returns argument
prop_testOneArgFunc :: Value -> Property
prop_testOneArgFunc v = eval Empty funcs (CallUserFunc "f" [Val v]) === Right v
    where
        func = UserFunc ["a"] [] (Var "a")
        funcs = insert ("f", func) Empty

-- Test function thats adds its two input arguments
prop_testTwoArgAddFunc :: NumberValue -> NumberValue -> Property
prop_testTwoArgAddFunc (NumberValue n1) (NumberValue n2) = eval Empty funcs (CallUserFunc "f" [v1, v2]) === expected
    where
        v1 = Val n1
        v2 = Val n2
        func = UserFunc ["a", "b"] [] (Add (Var "a") (Var "b"))
        funcs = insert ("f", func) Empty
        expected = evalBasic (Add v1 v2)

-- Test function that creates changes the value of a variable in its local function scope
-- i.e
-- f(a,b) { a = a + b; a }
prop_setVarInFunc :: NumberValue -> NumberValue -> Property
prop_setVarInFunc (NumberValue n1) (NumberValue n2) = eval Empty funcs (CallUserFunc "f" [v1, v2]) === expected
    where
        v1 = Val n1
        v2 = Val n2
        func = UserFunc ["a", "b"] [FuncSetVar "a" (Add (Var "a") (Var "b"))] (Var "a")
        funcs = insert ("f", func) Empty
        expected = evalBasic (Add v1 v2)


return []
runOperationTests = $quickCheckAll

{-# LANGUAGE TemplateHaskell #-}
module TestOperations where

import Test.QuickCheck
import Data.Fixed (divMod')

import Expr
import Error
import Eval
import BinaryTree

import Test

-- Either IntVal or FloatVal
newtype NVal = NVal Value
    deriving (Show)

instance Arbitrary NVal where
    arbitrary = fmap NVal (oneof [
        fmap (IntVal) arbitrary,
        fmap (FloatVal) arbitrary
        ])

-- Convert a Value (NVal) to a Float
numToFloat :: Value -> Float
numToFloat v = case v of
                IntVal i -> fromIntegral i
                FloatVal f -> f

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
checkEval2Int :: TwoArgOperation -> TwoIntOperation -> Int -> Int -> Property
checkEval2Int opExpr fResult x y = evalBasic expr === Right expected
    where
        expr = opExpr (Val (IntVal x)) (Val (IntVal y))
        expected = IntVal (fResult x y)

checkEval2Float :: TwoArgOperation -> TwoFloatOperation -> Float -> Float -> Property
checkEval2Float opExpr fResult x y = if (isNaN expectedFloat || isInfinite expectedFloat)
                                        then ensureMathError res
                                        else res === Right (FloatVal expectedFloat)
    where
        expr = opExpr (Val (FloatVal x)) (Val (FloatVal y))
        res = evalBasic expr
        expectedFloat = fResult x y


{- Check a mathematical function with two integer or float arguments that
 - When given 2 ints, returns an integer
 - When given 1 or 2 floats, returns a float
 - When given anything else, gives a value error
 -
 - Additionally, if the float operation returns NaN or Infinity, then we expect a MathError
 -}
checkMixedMath :: TwoArgOperation
                  -> TwoIntOperation -> TwoFloatOperation
                  -> Value -> Value
                  -> Property
checkMixedMath expr intOp floatOp (IntVal a)   (IntVal b)   = collect ("int int") $ checkEval2Int   expr intOp a b
checkMixedMath expr intOp floatOp (FloatVal a) (IntVal b)   = collect ("float int") $ checkEval2Float expr floatOp a (fromIntegral b)
checkMixedMath expr intOp floatOp (IntVal a)   (FloatVal b) = collect ("int float") $ checkEval2Float expr floatOp (fromIntegral a) b
checkMixedMath expr intOp floatOp (FloatVal a) (FloatVal b) = collect ("float float") $ checkEval2Float expr floatOp a b
checkMixedMath expr intOp floatOp a b = collect ("bad args") $ checkEvalValueError $ expr (Val a) (Val b)

{- Addition a + b -}
prop_testEvalAdd :: Value -> Value -> Property
prop_testEvalAdd = checkMixedMath Add (+) (+)

{- Subtraction a - b -}
prop_testEvalSub :: Value -> Value -> Property
prop_testEvalSub = checkMixedMath Sub (-) (-)

{- Multiplication a * b -}
prop_testEvalMul :: Value -> Value -> Property
prop_testEvalMul = checkMixedMath Mul (*) (*)

{- Division a / b -}
-- Define zero to shorten later code
zero_i_expr :: Expr
zero_i_expr = Val (IntVal 0)
zero_f_expr :: Expr
zero_f_expr = Val (FloatVal 0.0)

prop_testDiv :: Value -> Value -> Property
prop_testDiv a (IntVal 0)     = checkEvalMathError  $ Div (Val a) zero_i_expr
prop_testDiv a (FloatVal 0.0) = checkEvalMathError $ (Div (Val a) zero_f_expr)
prop_testDiv (IntVal a) (IntVal b) = a `mod` b /= 0 ==>
                                     checkEval2Float Div (/) (fromIntegral a) (fromIntegral b)
prop_testDiv a b = checkMixedMath Div (div) (/) a b

-- Check integer division when they divide perfectly
prop_testIntDiv :: Int -> Int -> Property
prop_testIntDiv a b = b /= 0 ==> checkEval2Int Div (div) (a*b) b

{- Modulo Tests -}

prop_testEvalMod :: Value -> Value -> Property
prop_testEvalMod a (IntVal 0)     = checkEvalMathError  $ Mod (Val a) zero_i_expr
prop_testEvalMod a (FloatVal 0.0) = checkEvalMathError  $ Mod (Val a) zero_f_expr
prop_testEvalMod a b              = checkMixedMath Mod (mod) (\x y -> snd (divMod' x y)) a b

{- Power AKA a^n tests -}
prop_testPow :: Value -> Value -> Property
prop_testPow (IntVal a) (IntVal n) | n < 0 = discard
                                   | otherwise = checkEval2Int Pow (^) a n
prop_testPow a n = checkMixedMath Pow (^) (**) a n

-- Test raising an int to a negative power (should result in a float)
prop_testEvalPowNegativeInt :: Int -> Negative Int -> Property
prop_testEvalPowNegativeInt a (Negative n) = a /= 0 ==> evalBasic expr === Right expected
    where
        expr = Pow (Val (IntVal a)) (Val (IntVal n))
        expected = FloatVal $ (fromIntegral a) ** (fromIntegral n)

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

prop_testEvalAbsPosNum :: NVal -> Property
prop_testEvalAbsPosNum (NVal v) = isVNonNeg v ==> evalBasic expr === Right v
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

prop_testEvalAbsValue :: Value -> Property
prop_testEvalAbsValue (IntVal i) = discard
prop_testEvalAbsValue (FloatVal f) = discard
prop_testEvalAbsValue v = ensureValueError $ evalBasic expr
    where
        expr = Abs (Val v)

{- Square Root sqrt() -}
prop_testSqrt :: Value -> Property
prop_testSqrt (StrVal v) = checkEvalValueError $ Sqrt (Val (StrVal v))
prop_testSqrt (BoolVal b) = checkEvalValueError $ Sqrt (Val (BoolVal b))
prop_testSqrt value | x < 0 = checkEvalMathError $ Sqrt (Val value)
                    | otherwise = evalBasic (Sqrt (Val value)) === Right expected
    where
        x = numToFloat value
        expected = FloatVal $ sqrt x

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
prop_testTwoArgAddFunc :: NVal -> NVal -> Property
prop_testTwoArgAddFunc (NVal n1) (NVal n2) = eval Empty funcs (CallUserFunc "f" [v1, v2]) === expected
    where
        v1 = Val n1
        v2 = Val n2
        func = UserFunc ["a", "b"] [] (Add (Var "a") (Var "b"))
        funcs = insert ("f", func) Empty
        expected = evalBasic (Add v1 v2)

-- Test function that creates changes the value of a variable in its local function scope
-- i.e
-- f(a,b) { a = a + b; a }
prop_setVarInFunc :: NVal -> NVal -> Property
prop_setVarInFunc (NVal n1) (NVal n2) = eval Empty funcs (CallUserFunc "f" [v1, v2]) === expected
    where
        v1 = Val n1
        v2 = Val n2
        func = UserFunc ["a", "b"] [FuncSetVar "a" (Add (Var "a") (Var "b"))] (Var "a")
        funcs = insert ("f", func) Empty
        expected = evalBasic (Add v1 v2)

{- Test concatenation of two strings -}
prop_evalConcat :: String -> String -> Bool
prop_evalConcat s1 s2 = evalBasic expr == Right expected
    where
        expr = Concat (Val (StrVal s1)) (Val (StrVal s2))
        expected = StrVal (s1 ++ s2)

-- Test concatenating not strings
prop_evalConcatNotString :: Value -> Value -> Property
prop_evalConcatNotString (StrVal _) _ = discard
prop_evalConcatNotString _ (StrVal _) = discard
prop_evalConcatNotString a b = checkEvalValueError $ Concat (Val a) (Val b)

{-- Equality and comparison operators --}
prop_evalEqualSelf :: Value -> Property
prop_evalEqualSelf v = evalBasic expr === Right (BoolVal True)
    where expr = IsEq (Val v) (Val v)

-- IsEq should always give opposite of NotEq
prop_evalEqualOppositeNotEq :: Value -> Value -> Property
prop_evalEqualOppositeNotEq v v1 = collect ("Eq: " ++ show (v == v1)) $
    case isEq of
         Right (BoolVal b) -> isNotEq === Right (BoolVal (not b))
         a -> counterexample ("isEq gave: " ++ show a) (False)
    where
        isEqExpr    = IsEq (Val v) (Val v)
        isNotEqExpr = NotEq (Val v) (Val v)
        isEq = evalBasic isEqExpr
        isNotEq = evalBasic isNotEqExpr

prop_compareNumbers :: NVal -> NVal -> Property
prop_compareNumbers (NVal n1) (NVal n2) = collect (show (compare l r)) $
    evalBasic (IsEq (Val n1) (Val n2))        === Right (BoolVal (l == r)) .&&.
    evalBasic (LessThan (Val n1) (Val n2))    === Right (BoolVal (l < r))  .&&.
    evalBasic (GreaterThan (Val n1) (Val n2)) === Right (BoolVal (l > r))
    where
        l = numToFloat n1
        r = numToFloat n2

return []
runOperationTests = $forAllProperties (quickCheckWithResult stdArgs { maxSuccess = 200 })

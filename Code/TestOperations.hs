{-# LANGUAGE TemplateHaskell #-}
module TestOperations where

import Test.QuickCheck

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
checkEval2Float opExpr fResult x y = evalBasic expr === Right expected
    where
        expr = opExpr (Val (FloatVal x)) (Val (FloatVal y))
        expected = FloatVal (fResult x y)

ensureMathError :: Show a => Either Error a -> Property
ensureMathError (Left (MathError _)) = property True
ensureMathError other = counterexample ("Expected MathError, got: " ++ show other) False

ensureValueError :: Show a => Either Error a -> Property
ensureValueError (Left (ValueError _)) = property True
ensureValueError other = counterexample ("Expected ValueError, got: " ++ show other) False

checkEvalMathError :: Expr -> Property
checkEvalMathError expr = ensureMathError $ evalBasic expr

checkEvalValueError :: Expr -> Property
checkEvalValueError expr = ensureValueError $ evalBasic expr

{- Addition a + b -}
prop_testEvalAdd :: Value -> Value -> Property
prop_testEvalAdd (StrVal a) b = checkEvalValueError $ Add (Val (StrVal a)) (Val b)
prop_testEvalAdd a (StrVal b) = checkEvalValueError $ Add (Val a) (Val (StrVal b))
prop_testEvalAdd (IntVal a) (IntVal b) = checkEval2Int Add (+) a b
prop_testEvalAdd a b = checkEval2Float Add (+) (numToFloat a) (numToFloat b)

{- Subtraction a - b -}
prop_testEvalSub :: Value -> Value -> Property
prop_testEvalSub (StrVal a) b = checkEvalValueError $ Sub (Val (StrVal a)) (Val b)
prop_testEvalSub a (StrVal b) = checkEvalValueError $ Sub (Val a) (Val (StrVal b))
prop_testEvalSub (IntVal a) (IntVal b) = checkEval2Int Sub (-) a b
prop_testEvalSub a b = checkEval2Float Sub (-) (numToFloat a) (numToFloat b)

{- Multiplication a * b -}
prop_testEvalMul :: Value -> Value -> Property
prop_testEvalMul (StrVal a) b = checkEvalValueError $ Mul (Val (StrVal a)) (Val b)
prop_testEvalMul a (StrVal b) = checkEvalValueError $ Mul (Val a) (Val (StrVal b))
prop_testEvalMul (IntVal a) (IntVal b) = checkEval2Int Mul (*) a b
prop_testEvalMul a b = checkEval2Float Mul (*) (numToFloat a) (numToFloat b)


{- Division a / b -}
-- Define zero to shorten later code
zero_i_expr :: Expr
zero_i_expr = Val (IntVal 0)
zero_f_expr :: Expr
zero_f_expr = Val (FloatVal 0.0)

prop_testDiv :: Value -> Value -> Property
prop_testDiv (StrVal a) b     = checkEvalValueError $ Div (Val (StrVal a)) (Val b)
prop_testDiv a (StrVal b)     = checkEvalValueError $ Div (Val a) (Val (StrVal b))
prop_testDiv a (IntVal 0)     = checkEvalMathError  $ Div (Val a) zero_i_expr
prop_testDiv a (FloatVal 0.0) = checkEvalMathError $ (Div (Val a) zero_f_expr)
prop_testDiv (IntVal a) (IntVal b) = a `mod` b /= 0 ==>
                                     checkEval2Float Div (/) (fromIntegral a) (fromIntegral b)
prop_testDiv a b = checkEval2Float Div (/) (numToFloat a) (numToFloat b)

-- Check integer division when they divide perfectly
prop_testIntDiv :: Int -> Int -> Property
prop_testIntDiv a b = b /= 0 ==> checkEval2Int Div (div) (a*b) b

{- Modulo Tests -}
-- TODO: modulo for floats?

prop_testEvalMod :: Value -> Value -> Property
prop_testEvalMod (StrVal a) b     = checkEvalValueError $ Mod (Val (StrVal a)) (Val b)
prop_testEvalMod a (StrVal b)     = checkEvalValueError $ Mod (Val a) (Val (StrVal b))
prop_testEvalMod a (IntVal 0)     = checkEvalMathError  $ Mod (Val a) zero_i_expr
-- prop_testEvalMod a (FloatVal 0.0) = checkEvalMathError $ (Div (Val a) zero_f_expr)
prop_testEvalMod (IntVal a) (IntVal b) = checkEval2Int Mod mod a b
prop_testEvalMod a b = checkEvalValueError $ Mod (Val a) (Val b)

{- Power AKA a^n tests -}
prop_testPow :: Value -> Value -> Property
prop_testPow (StrVal a) n     = checkEvalValueError $ Mod (Val (StrVal a)) (Val n)
prop_testPow a (StrVal n)     = checkEvalValueError $ Mod (Val a) (Val (StrVal n))
prop_testPow (IntVal a) (IntVal n) | a == 0 && n < 0  = checkEvalMathError $ Pow (Val (IntVal a)) (Val (IntVal n)) -- Divide by zero
                                   | n < 0            = checkEval2Float Pow (**) (fromIntegral a) (fromIntegral n) -- Fraction
                                   | otherwise        = checkEval2Int Pow (^) a n
prop_testPow a (IntVal n) = checkEval2Float Pow (**) (numToFloat a) (fromIntegral n)
prop_testPow aV nV | a <= 0.0  = checkEvalMathError $ Pow (Val aV) (Val nV) -- Divide by zero OR sqrt-like negative
                   | otherwise = checkEval2Float Pow (**) a n
    where
        a = numToFloat aV
        n = numToFloat nV

prop_testEvalPowIntNon0 :: Int -> Int -> Property
prop_testEvalPowIntNon0 a x = a /= 0 ==> checkEval2Int Pow (^) a x

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

prop_testEvalPosNum :: NVal -> Property
prop_testEvalPosNum (NVal v) = isVNonNeg v ==> evalBasic expr === Right v
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
prop_testSqrt :: Value -> Property
prop_testSqrt (StrVal v) = checkEvalValueError $ Sqrt (Val (StrVal v))
prop_testSqrt value | x < 0 = checkEvalMathError $ Sqrt (Val value)
                    | otherwise = evalBasic (Sqrt (Val value)) === Right expected
    where
        x = numToFloat value
        expected = FloatVal $ x ** 0.5

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

return []
runOperationTests = $quickCheckAll

{-# LANGUAGE TemplateHaskell #-}
module Test where

import Test.QuickCheck


import Expr

-- A Value thats always generated to be an IntVal
newtype RIntVal = RIntVal Value
-- A value thats always generated to be an RIntVal
newtype RStrVal = RStrVal Value

instance Arbitrary RIntVal where
    arbitrary = fmap (RIntVal . IntVal) arbitrary

instance Arbitrary RStrVal where
    arbitrary = fmap (RStrVal . StrVal) arbitrary


instance Arbitrary Value where
    arbitrary = oneof [
        fmap (StrVal) arbitrary,
        fmap (IntVal) arbitrary
        ]

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

prop_testEvalDiv :: Int -> Int -> Bool
prop_testEvalDiv = testEval2IntArithmetic Mul (\x y -> x `div` y)

return []
runTests = $quickCheckAll

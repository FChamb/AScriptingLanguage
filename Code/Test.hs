{-# LANGUAGE TemplateHaskell #-}
module Test where

import Test.QuickCheck

import Data.List
import Data.Maybe
import Text.Read

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

newtype ValidName = ValidName Name
    deriving (Show)

instance Arbitrary ValidName where
    arbitrary = do firstLetter <- elements lowerLetters
                   remaining <- listOf validRemaining
                   return $ ValidName (firstLetter:remaining)
        where
            lowerLetters = ['a'..'z']
            validRemaining = frequency [
                (5, elements lowerLetters)
                , (5, elements ['A'..'Z'])
                , (1, elements ['0'..'9'])]


-- Test direct values
prop_testEvalVal :: Value -> Bool
prop_testEvalVal value = eval [] expr == Just value
    where expr = Val (value)

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
prop_testEvalDiv :: Int -> Int -> Property
prop_testEvalDiv a b = b /= 0 ==> testEval2IntArithmetic Div div a b

-- TOOD: Test divisor == 0
prop_testEvalMod :: Int -> Int -> Property
prop_testEvalMod a b = b /= 0 ==> testEval2IntArithmetic Mod mod a b

-- TODO: Test power < 0
prop_testEvalPow :: Int -> Positive Int -> Bool
prop_testEvalPow a (Positive b) = testEval2IntArithmetic Pow (^) a b

prop_testEvalAbs :: Positive Int -> Bool
prop_testEvalAbs (Positive i) = eval [] expr == Just expected
    where
        expr = (Abs . Val . IntVal) (-i)
        expected = IntVal i

-- Variables
prop_getDefinedVar :: ValidName -> Value -> Bool
prop_getDefinedVar (ValidName name) value = eval [(name, value)] (Var name) == Just value

prop_getDefinedVarFromMultiple :: [(ValidName, Value)] -> Property
prop_getDefinedVarFromMultiple inputVars = do
        not (null uniqueVars) ==> do
            (name, value) <- elements uniqueVars
            return $ eval uniqueVars (Var name) == Just value
    where
        vars = map (\(ValidName n, v) -> (n,v)) inputVars
        uniqueVars = nubBy (\(n1, v1) (n2, v2) -> n1 == n2) vars

prop_getUndefinedVar :: ValidName -> Bool
prop_getUndefinedVar (ValidName name) = eval [] (Var name) == Nothing

-- Type conversion

prop_evalToString :: Value -> Bool
prop_evalToString (IntVal i) = eval [] expr == Just expected
    where
        expr = ToString (Val (IntVal i))
        expected = StrVal (show i)
prop_evalToString (StrVal s) = eval [] expr == Just expected
    where
        expr = ToString (Val (StrVal s))
        expected = StrVal s

{- ToInt checks -}
-- Test all kinds of values
prop_evalValueToInt :: Value -> Property
prop_evalValueToInt (IntVal i) = eval [] expr === Just expected
    where
        expr = ToInt (Val (IntVal i))
        expected = IntVal i
-- Test random strings (usually not valid)
prop_evalValueToInt (StrVal s) = isNothing validInt ==> eval [] expr == Nothing
    where
        validInt :: Maybe Int
        validInt = readMaybe s
        expr = ToInt (Val (StrVal s))

-- Test strings that are definitely integers
prop_evalIntStringToInt :: Int -> Bool
prop_evalIntStringToInt i = eval [] expr == Just expected
    where
        expr = (ToInt . Val . StrVal . show) i
        expected = IntVal i

-- Test concatenation of two strings
prop_evalConcat :: String -> String -> Bool
prop_evalConcat s1 s2 = eval [] expr == Just expected
    where
        expr = Concat (Val (StrVal s1)) (Val (StrVal s2))
        expected = StrVal (s1 ++ s2)

-- More advanced expression tree tests

-- Try addition using variables
prop_evalAddWithVars :: (ValidName, Int) -> (ValidName, Int) -> Bool
prop_evalAddWithVars (ValidName na, a) (ValidName nb, b) = eval vars expr == Just (IntVal expected)
    where
        vars = [(na, IntVal a), (nb, IntVal b)]
        expr = Add (Var na) (Var nb)
        expected = a + b

-- Try multiplication and addition in expression tree
prop_evalAddAndMul :: Int -> Int -> Int -> Bool
prop_evalAddAndMul a b c = eval [] expr == Just (IntVal expected)
    where
        subExpr :: Expr
        subExpr = Mul (Val (IntVal b)) (Val (IntVal c))
        expr :: Expr
        expr = Add (Val (IntVal a)) subExpr
        expected = a + (b*c)

return []
runTests = $quickCheckAll

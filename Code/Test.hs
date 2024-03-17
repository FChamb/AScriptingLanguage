{-# LANGUAGE TemplateHaskell #-}
module Test where

import Test.QuickCheck

import Data.List

import Expr
import Eval

instance Arbitrary Value where
    arbitrary = oneof [
        fmap (StrVal) arbitrary,
        fmap (IntVal) arbitrary,
        fmap (FloatVal) arbitrary
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


-- Simple wrapper for calling eval with no defined vars or funcs
evalBasic :: Expr -> Maybe Value
evalBasic = eval [] []


-- Test direct values
prop_testEvalVal :: Value -> Bool
prop_testEvalVal value = evalBasic expr == Just value
    where expr = Val (value)


-- Variables
prop_getDefinedVar :: ValidName -> Value -> Bool
prop_getDefinedVar (ValidName name) value = eval [(name, value)] [] (Var name) == Just value

prop_getDefinedVarFromMultiple :: [(ValidName, Value)] -> Property
prop_getDefinedVarFromMultiple inputVars = do
        not (null uniqueVars) ==> do
            (name, value) <- elements uniqueVars
            return $ eval uniqueVars [] (Var name) == Just value
    where
        vars = map (\(ValidName n, v) -> (n,v)) inputVars
        uniqueVars = nubBy (\(n1, v1) (n2, v2) -> n1 == n2) vars

prop_getUndefinedVar :: ValidName -> Bool
prop_getUndefinedVar (ValidName name) = evalBasic (Var name) == Nothing




{- Test concatenation of two strings -}
prop_evalConcat :: String -> String -> Bool
prop_evalConcat s1 s2 = evalBasic expr == Just expected
    where
        expr = Concat (Val (StrVal s1)) (Val (StrVal s2))
        expected = StrVal (s1 ++ s2)

-- More advanced expression tree tests

-- Try addition using variables
prop_evalAddWithVars :: (ValidName, Int) -> (ValidName, Int) -> Bool
prop_evalAddWithVars (ValidName na, a) (ValidName nb, b) = eval vars [] expr == Just (IntVal expected)
    where
        vars = [(na, IntVal a), (nb, IntVal b)]
        expr = Add (Var na) (Var nb)
        expected = a + b

-- Try multiplication and addition in expression tree
prop_evalAddAndMul :: Int -> Int -> Int -> Bool
prop_evalAddAndMul a b c = evalBasic expr == Just (IntVal expected)
    where
        subExpr :: Expr
        subExpr = Mul (Val (IntVal b)) (Val (IntVal c))
        expr :: Expr
        expr = Add (Val (IntVal a)) subExpr
        expected = a + (b*c)

return []
runMainTests = $quickCheckAll

{-# LANGUAGE TemplateHaskell #-}
module TestConversion where

import Text.Read
import Data.Maybe

import Test.QuickCheck

import Expr
import Test

-- Type conversion

{- ToString checks -}
prop_evalToString :: Value -> Bool
prop_evalToString (IntVal i) = evalBasic expr == Just expected
    where
        expr = ToString (Val (IntVal i))
        expected = StrVal (show i)
prop_evalToString (FloatVal f) = evalBasic expr == Just expected
    where
        expr = ToString (Val (FloatVal f))
        expected = StrVal (show f)
prop_evalToString (StrVal s) = evalBasic expr == Just expected
    where
        expr = ToString (Val (StrVal s))
        expected = StrVal s

{- ToInt checks -}
-- Test all kinds of values
prop_evalValueToInt :: Value -> Property
prop_evalValueToInt (IntVal i) = evalBasic expr === Just expected
    where
        expr = ToInt (Val (IntVal i))
        expected = IntVal i
prop_evalValueToInt (FloatVal f) = evalBasic expr === Just expected
    where
        expr = ToInt (Val (FloatVal f))
        expected = IntVal (truncate f)
-- Test random strings (usually not valid)
prop_evalValueToInt (StrVal s) = isNothing validInt ==> evalBasic expr == Nothing
    where
        validInt :: Maybe Int
        validInt = readMaybe s
        expr = ToInt (Val (StrVal s))

-- Test strings that are definitely integers
prop_evalIntStringToInt :: Int -> Bool
prop_evalIntStringToInt i = evalBasic expr == Just expected
    where
        expr = (ToInt . Val . StrVal . show) i
        expected = IntVal i

{-- ToFloat checks --}
-- Test all kinds of values
prop_evalValueToFloat :: Value -> Property
prop_evalValueToFloat (IntVal i) = evalBasic expr === Just expected
    where
        expr = ToFloat (Val (IntVal i))
        expected = FloatVal (fromIntegral i)
prop_evalValueToFloat (FloatVal f) = evalBasic expr === Just expected
    where
        expr = ToFloat (Val (FloatVal f))
        expected = FloatVal f
-- Test random strings (usually not valid)
prop_evalValueToFloat (StrVal s) = isNothing validFloat ==> evalBasic expr == Nothing
    where
        validFloat :: Maybe Float
        validFloat = readMaybe s
        expr = ToFloat (Val (StrVal s))

-- Test strings that are definitely floats
prop_evalFloatStringToFloat :: Float -> Bool
prop_evalFloatStringToFloat f = evalBasic expr == Just expected
    where
        expr = (ToFloat . Val . StrVal . show) f
        expected = FloatVal f

return []
runConversionTests = $quickCheckAll

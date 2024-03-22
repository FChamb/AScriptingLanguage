{-# LANGUAGE TemplateHaskell #-}
module TestConversion where

import Text.Read
import Data.Maybe

import Test.QuickCheck

import Expr
import Error
import Test

-- Type conversion

{- ToString checks -}
prop_evalToString :: Value -> Bool
prop_evalToString (IntVal i) = evalBasic expr == Right expected
    where
        expr = ToString (Val (IntVal i))
        expected = StrVal (show i)
prop_evalToString (FloatVal f) = evalBasic expr == Right expected
    where
        expr = ToString (Val (FloatVal f))
        expected = StrVal (show f)
prop_evalToString (StrVal s) = evalBasic expr == Right expected
    where
        expr = ToString (Val (StrVal s))
        expected = StrVal s
prop_evalToString (BoolVal b) = evalBasic expr == Right expected
    where
        expr = ToString (Val (BoolVal b))
        expected = BoolVal $ if b then True else False

{- ToInt checks -}
-- Test all kinds of values
prop_evalValueToInt :: Value -> Property
prop_evalValueToInt (IntVal i) = evalBasic expr === Right expected
    where
        expr = ToInt (Val (IntVal i))
        expected = IntVal i
prop_evalValueToInt (FloatVal f) = evalBasic expr === Right expected
    where
        expr = ToInt (Val (FloatVal f))
        expected = IntVal (truncate f)
prop_evalValueToInt (BoolVal b) = evalBasic expr === Right expected
    where
        expr = ToInt (Val (BoolVal b))
        expected = IntVal $ if b then 1 else 0
-- Test random strings (usually not valid)
prop_evalValueToInt (StrVal s) = isNothing validInt ==> case evalBasic expr of
                                                             Left (ValueError _) -> True
                                                             _ -> False
    where
        validInt :: Maybe Int
        validInt = readMaybe s
        expr = ToInt (Val (StrVal s))

-- Test strings that are definitely integers
prop_evalIntStringToInt :: Int -> Bool
prop_evalIntStringToInt i = evalBasic expr == Right expected
    where
        expr = (ToInt . Val . StrVal . show) i
        expected = IntVal i

{-- ToFloat checks --}
-- Test all kinds of values
prop_evalValueToFloat :: Value -> Property
prop_evalValueToFloat (IntVal i) = evalBasic expr === Right expected
    where
        expr = ToFloat (Val (IntVal i))
        expected = FloatVal (fromIntegral i)
prop_evalValueToFloat (FloatVal f) = evalBasic expr === Right expected
    where
        expr = ToFloat (Val (FloatVal f))
        expected = FloatVal f
prop_evalValueToFloat (BoolVal b) = ensureValueError $ evalBasic expr
    where expr = ToFloat (Val (BoolVal b))
-- Test random strings (usually not valid)
prop_evalValueToFloat (StrVal s) = isNothing validFloat ==> ensureValueError $ evalBasic expr 
    where
        validFloat :: Maybe Float
        validFloat = readMaybe s
        expr = ToFloat (Val (StrVal s))

-- Test strings that are definitely floats
prop_evalFloatStringToFloat :: Float -> Bool
prop_evalFloatStringToFloat f = evalBasic expr == Right expected
    where
        expr = (ToFloat . Val . StrVal . show) f
        expected = FloatVal f

return []
runConversionTests = $quickCheckAll

module Eval where

import Text.Read (readMaybe)
import Data.Fixed (divMod') 

import Expr
import Error
import BinaryTree

eval :: Vars -> -- Variable name to value mapping
        Funcs -> -- User defined functions
        Expr -> -- Expression to be evaluated
        Either Error Value -- Result (if no errors such as missing variables)

-- Value operations 

eval vars fs (Val x) = Right x -- for values, just give the value directly
eval vars fs (Var name) = case value name vars of -- for variables, give if exists
                               Right x -> Right x
                               Left _ -> Left $ ValueError("Value does not exist")

{- Evaluates concatenation of values, supports StrVals
   ValueError for BoolVals, IntVals, FloatVals -}
eval vars fs (Concat a b) = case (eval vars fs a, eval vars fs b) of
                                (Right (StrVal x), Right (StrVal y)) -> Right ((StrVal (x ++ y)))
                                _ -> Left $ ValueError "Can only concatenate strings"                            

-- Mathematical operations 

-- Evaluates addition, supports combinations of integers and floats, ValueError for BoolVals or StrVals
eval vars fs (Add x y) = do
    xVal <- eval vars fs x
    yVal <- eval vars fs y
    case (xVal, yVal) of
        (IntVal intA, IntVal intB) -> Right (IntVal (intA + intB))
        (FloatVal fltA, FloatVal fltB) -> Right (FloatVal (fltA + fltB))
        (IntVal int, FloatVal flt) -> Right (FloatVal (fromIntegral int + flt))
        (FloatVal flt, IntVal int) -> Right (FloatVal (flt + fromIntegral int))
        _ -> Left $ ValueError "Cannot add given values"

-- Evaluates substraction, supports combinations of integers and floats, ValueError for BoolVals or StrVals
eval vars fs (Sub x y) = do
    xVal <- eval vars fs x
    yVal <- eval vars fs y
    case (xVal, yVal) of
        (IntVal intA, IntVal intB) -> Right (IntVal (intA - intB))
        (FloatVal fltA, FloatVal fltB) -> Right (FloatVal (fltA - fltB))
        (IntVal int, FloatVal flt) -> Right (FloatVal (fromIntegral int - flt))
        (FloatVal flt, IntVal int) -> Right (FloatVal (flt - fromIntegral int))
        _ -> Left $ ValueError "Cannot subtract given values"

-- Evaluates multiplication, supports combinations of integers and floats, ValueError for BoolVals or StrVals
eval vars fs (Mul x y) = do
    xVal <- eval vars fs x
    yVal <- eval vars fs y
    case (xVal, yVal) of
        (IntVal intA, IntVal intB) -> Right (IntVal (intA * intB))
        (FloatVal fltA, FloatVal fltB) -> Right (FloatVal (fltA * fltB))
        (IntVal int, FloatVal flt) -> Right (FloatVal (fromIntegral int * flt))
        (FloatVal flt, IntVal int) -> Right (FloatVal (flt * fromIntegral int))
        _ ->  Left $ ValueError "Cannot multiply given values"

{- Evaluates division, supports combinations of integers and floats
   MathError for division by 0, ValueError for BoolVals or StrVals -}
eval vars fs (Div x y) = do
    xVal <- eval vars fs x
    yVal <- eval vars fs y
    case (xVal, yVal) of
        (_, IntVal 0) -> Left $ MathError "Division by 0 is undefined"
        (_, FloatVal 0.0) -> Left $ MathError "Division by 0 is undefined"
        (IntVal intA, IntVal intB) -> if (intA `mod` intB == 0)
                                            then Right (IntVal (intA `div` intB))
                                            else Right (FloatVal (fromIntegral intA / fromIntegral intB))
        (FloatVal fltA, FloatVal fltB) -> Right (FloatVal (fltA / fltB))
        (IntVal int, FloatVal flt) -> Right (FloatVal (fromIntegral int / flt))
        (FloatVal flt, IntVal int) -> Right (FloatVal (flt / fromIntegral int))
        _ -> Left $ ValueError "Cannot divide given values"

-- Evaluates absolute value, supports integers and floats, ValueError for BoolVals or StrVals
eval vars fs (Abs a) = do
    val <- eval vars fs a
    case val of
        IntVal int -> Right (IntVal (abs int))
        FloatVal flt -> Right (FloatVal (abs flt))
        _ -> Left $ ValueError "Cannot calculate absolute value of given value"

{- Evaluates modulo, supports combinations of integers and floats
   MathError for modulo by 0, ValueError for BoolVals or StrVals -}
eval vars fs (Mod x y) = do
    dividend <- eval vars fs x
    divisor <- eval vars fs y
    case (dividend, divisor) of
        (_, IntVal 0) -> Left $ MathError "Cannot do modulo with 0"
        (_, FloatVal 0.0) -> Left $ MathError "Cannot do modulo with 0"
        (IntVal intA, IntVal intB) -> return (IntVal (intA `mod` intB))
        (FloatVal fltA, FloatVal fltB) -> return (FloatVal (snd (divMod' fltA fltB)))
        (IntVal int, FloatVal flt) -> return (FloatVal (snd (divMod' (fromIntegral int) flt)))
        (FloatVal flt, IntVal int) -> return (FloatVal (snd (divMod' flt (fromIntegral int))))
        _ -> Left $ ValueError "Cannot caclulate modulo of given values"

{- Evaluates powers, supports combinations of integers and floats
   MathError for division by 0, ValueError for BoolVals or StrVals -}
eval vars fs (Pow x y) = do
    base <- eval vars fs x
    exp <- eval vars fs y
    case (base, exp) of
        (IntVal intA, IntVal intB) | intA == 0 && intB < 0 -> Left $ MathError "Division by 0 is undefined"
                                   | intB < 0              -> Right $ FloatVal ((fromIntegral intA) ** (fromIntegral intB)) -- negative exponent == float
                                   | otherwise             -> Right (IntVal (intA ^ intB)) -- ^ operator solely used for integers
        (FloatVal fltA, FloatVal fltB) | isNaN $ fltA ** fltB -> Left $ MathError "Program does not support imaginary numbers"
                                       | isInfinite $ fltA ** fltB -> Left $ MathError "Division by 0 is undefined"
                                       | otherwise -> Right (FloatVal (fltA ** fltB))
        (IntVal int, FloatVal flt) | isNaN $ (fromIntegral int) ** flt -> Left $ MathError "Program does not support imaginary numbers"
                                   | isInfinite $ (fromIntegral int) ** flt -> Left $ MathError "Division by 0 is undefined"
                                   | otherwise -> Right (FloatVal (fromIntegral int ** flt))
        (FloatVal flt, IntVal int) | flt == 0.0 && int < 0 -> Left $ MathError "Division by 0 is undefined"
                                   | otherwise -> Right (FloatVal (flt ** fromIntegral int))
        _ -> Left $ ValueError "Cannot calculate power of given values"

{- Evaluates square root, supports integers and floats
   MathError for negatives, ValueError for BoolVals or StrVals -}
eval vars fs (Sqrt a) = do
    val <- eval vars fs a
    case val of
        IntVal int | int < 0 -> Left $ MathError "Negative are not supported for this operation"
                   | otherwise -> Right (FloatVal (sqrt $ fromIntegral int))
        FloatVal flt | flt < 0.0 ->  Left $ MathError "Negative are not supported for this operation"
                     | otherwise -> Right (FloatVal (sqrt flt))
        _ -> Left $ ValueError "Cannot calculate square root of given value"

{- User defined functions
 -
 - Here we:
 - 1. Evaluate all arguments passed in
 - 2. Zip their values with the argument names they should be bound to
 - 3. Turn the list of arguments and values into a tree
 - 4. Evaluate all the statements in the function to get a new state
 - 5. Evaluate the final expression, returning its value
 -}
eval vars fs (CallUserFunc name args) = do
    (UserFunc fargs stmts retExpr) <- case value name fs of
        Left _ -> Left $ ValueError ("Function " ++ name ++ " doesn not exist")
        Right f -> Right f
    if length args == length fargs
       then do evaledArgs <- sequence $ map (eval vars fs) args
               let fState = treeFromList $ zip fargs evaledArgs
               endState <- foldl (evalStmt) (Right fState) stmts
               eval endState fs retExpr
       else Left (ParseError ("Wrong number of arguments to function " ++ name
                               ++ ", got " ++ show (length args)
                               ++ " expected: " ++ show (length fargs)))
    where
        evalStmt :: Either Error Vars -> FuncStatement -> Either Error Vars
        evalStmt vars (FuncSetVar n e) = vars >>= updateEval n e
        updateEval :: Name -> Expr -> Vars -> Either Error Vars
        updateEval n e vs = eval vs fs e >>= \v -> Right $ insert (n, v) vs

-- Conversions 

-- Convert given value to StrVal, if error in given expression, propagates up
eval vars fs (ToString e) = do
    case eval vars fs e of
        Right (StrVal s) -> Right $ StrVal s
        Right (IntVal i) -> Right $ StrVal (show i)
        Right (FloatVal f) -> Right $ StrVal (show f)
        Right (BoolVal b) -> Right $ StrVal (show b)
        Left err -> Left $ err -- Passes up error raised inside of toString call

{- Convert given value to IntVal, if error in given expression, propagates up.
   ValueError for non-numeric strings -}
eval vars fs (ToInt e) = do
    value <- eval vars fs e
    case value of
        StrVal s -> case readMaybe s of 
                        Just x -> Right (IntVal x)
                        Nothing -> Left (ValueError "Cannot convert given value to int")
        BoolVal b -> case b of
                        True -> Right (IntVal 1) -- 1 for true
                        False -> Right (IntVal 0) -- 0 for false
        IntVal i -> Right (IntVal i)
        FloatVal f -> Right (IntVal (truncate f))

{- Convert given value to FloatVal, if error in given expression, propagates up.
   ValueError for booleans & non-numeric strings -}
eval vars fs (ToFloat e) = do
    value <- eval vars fs e
    case value of
        StrVal s -> case readMaybe s of
                        Just x -> Right (FloatVal x)
                        Nothing -> Left (ValueError "Cannot convert string to float")
        IntVal i -> Right (FloatVal (fromIntegral i))
        FloatVal f -> Right (FloatVal f)
        BoolVal b -> Left $ ValueError "Cannot convert boolean to float"

-- Boolean operations

-- Equality
eval vars fs (IsEq a b) = do
    aV <- eval vars fs a
    bV <- eval vars fs b
    return $ BoolVal $ checkExprEq aV bV

-- Inequality
eval vars fs (NotEq a b) = do
    aV <- eval vars fs a
    bV <- eval vars fs b
    return $ BoolVal $ not $ checkExprEq aV bV

-- Less than, ValueError for bools and strings
eval vars fs (LessThan a b) = do
    aV <- eval vars fs a
    bV <- eval vars fs b
    case (aV, bV) of
        (IntVal left, IntVal right) -> Right $ BoolVal $ left < right
        (FloatVal left, IntVal right) -> Right $ BoolVal $ left < (fromIntegral right)
        (IntVal left, FloatVal right) -> Right $ BoolVal $ (fromIntegral left) < right
        (FloatVal left, FloatVal right) -> Right $ BoolVal $ left < right
        _ -> Left $ ValueError "Uncomparable types"

-- Greater than, ValueError for bools and strings 
eval vars fs (GreaterThan a b) = do
    aV <- eval vars fs a
    bV <- eval vars fs b
    case (aV, bV) of
        (IntVal left, IntVal right) -> Right $ BoolVal $ left > right
        (FloatVal left, IntVal right) -> Right $ BoolVal $ left > (fromIntegral right)
        (IntVal left, FloatVal right) -> Right $ BoolVal $ (fromIntegral left) > right
        (FloatVal left, FloatVal right) -> Right $ BoolVal $ left > right
        _ -> Left $ ValueError "Uncomparable types"

-- Helper function that checks equality and returns a boolean
checkExprEq :: Value -> Value -> Bool
checkExprEq a b = do
    case (a, b) of
        (StrVal left, StrVal right) -> left == right
        (IntVal left, IntVal right) -> left == right
        (FloatVal left, IntVal right) -> left == (fromIntegral right)
        (IntVal left, FloatVal right) -> (fromIntegral left) == right
        (FloatVal left, FloatVal right) -> left == right
        (BoolVal left, BoolVal right) -> left == right
        _ -> False

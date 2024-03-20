module Eval where

import Text.Read
import Data.Maybe
import Data.Fixed (divMod') -- For divMod'

import Expr
import BinaryTree

eval :: Vars -> -- Variable name to value mapping
        Funcs ->
        Expr -> -- Expression to evaluate
        Either Error Value -- Result (if no errors such as missing variables)
eval vars fs (Val x) = Right x -- for values, just give the value directly
eval vars fs (Var name) = case value name vars of -- for values, just give the value directly
                               Right x -> Right x
                               Left _ -> Left $ ValueError("value does not exist")
                                
eval vars fs (Add x y) = do
    xVal <- eval vars fs x
    yVal <- eval vars fs y
    case (xVal, yVal) of
        (IntVal intA, IntVal intB) -> Right (IntVal (intA + intB))
        (FloatVal fltA, FloatVal fltB) -> Right (FloatVal (fltA + fltB))
        (IntVal int, FloatVal flt) -> Right (FloatVal (fromIntegral int + flt))
        (FloatVal flt, IntVal int) -> Right (FloatVal (flt + fromIntegral int))
        _ -> Left $ MathError "flawed addition operation"

eval vars fs (Sub x y) = do
    xVal <- eval vars fs x
    yVal <- eval vars fs y
    case (xVal, yVal) of
        (IntVal intA, IntVal intB) -> Right (IntVal (intA - intB))
        (FloatVal fltA, FloatVal fltB) -> Right (FloatVal (fltA - fltB))
        (IntVal int, FloatVal flt) -> Right (FloatVal (fromIntegral int - flt))
        (FloatVal flt, IntVal int) -> Right (FloatVal (flt - fromIntegral int))
        _ -> Left $ MathError "flawed subtraction operation"

eval vars fs (Mul x y) = do
    xVal <- eval vars fs x
    yVal <- eval vars fs y
    case (xVal, yVal) of
        (IntVal intA, IntVal intB) -> Right (IntVal (intA * intB))
        (FloatVal fltA, FloatVal fltB) -> Right (FloatVal (fltA * fltB))
        (IntVal int, FloatVal flt) -> Right (FloatVal (fromIntegral int * flt))
        (FloatVal flt, IntVal int) -> Right (FloatVal (flt * fromIntegral int))
        _ ->  Left $ MathError "flawed multiplication operation"

eval vars fs (Div x y) = do
    xVal <- eval vars fs x
    yVal <- eval vars fs y
    case (xVal, yVal) of
        (_, IntVal 0) -> Left $ ValueError "program does not support imaginary numbers"
        (_, FloatVal 0.0) -> Left $ ValueError "program does not support imaginary numbers"
        (IntVal intA, IntVal intB) -> if (intA `mod` intB == 0)
                                            then Right (IntVal (intA `div` intB))
                                            else Right (FloatVal (fromIntegral intA / fromIntegral intB))
        (FloatVal fltA, FloatVal fltB) -> Right (FloatVal (fltA / fltB))
        (IntVal int, FloatVal flt) -> Right (FloatVal (fromIntegral int / flt))
        (FloatVal flt, IntVal int) -> Right (FloatVal (flt / fromIntegral int))
        _ -> Left $ MathError "flawed division operation"

eval vars fs (Concat a b) = case (eval vars fs a, eval vars fs b) of
                                (Right (StrVal x), Right (StrVal y)) -> Right ((StrVal (x ++ y)))
                                _ -> Left $ ValueError "cannot concatenate"

eval vars fs (Abs a) = do
    val <- eval vars fs a
    case val of
        IntVal int -> Right (IntVal (abs int))
        FloatVal flt -> Right (FloatVal (abs flt))
        _ -> Left $ MathError "flawed absolute value operation"

eval vars fs (Mod x y) = do
    dividend <- eval vars fs x
    divisor <- eval vars fs y
    case (dividend, divisor) of
        (_, IntVal 0) -> Left $ MathError "cannot do modulo with 0"
        (_, FloatVal 0.0) -> Left $ MathError "cannot do modulo with 0"
        (IntVal intA, IntVal intB) -> return (IntVal (intA `mod` intB))
        (FloatVal fltA, FloatVal fltB) -> return (FloatVal (snd (divMod' fltA fltB)))
        (IntVal int, FloatVal flt) -> return (FloatVal (snd (divMod' (fromIntegral int) flt)))
        (FloatVal flt, IntVal int) -> return (FloatVal (snd (divMod' flt (fromIntegral int))))
        _ -> Left $ MathError "flawed modulo operation"

eval vars fs (Pow x y) = do
    base <- eval vars fs x
    exp <- eval vars fs y
    case (base, exp) of
        (IntVal intA, IntVal intB) -> Right (IntVal (intA ^ intB))
        (FloatVal fltA, FloatVal fltB) -> Right (FloatVal (fltA ** fltB))
        (IntVal int, FloatVal flt) -> Right (FloatVal (fromIntegral int ** flt))
        (FloatVal flt, IntVal int) -> Right (FloatVal (flt ** fromIntegral int))

eval vars fs (Sqrt a) = do
    val <- eval vars fs a
    case val of
        IntVal (-1) -> Left $ MathError "program does not support imaginary numbers"
        IntVal int -> Right (FloatVal (sqrt $ fromIntegral int))
        FloatVal flt -> Right (FloatVal (sqrt flt))
        _ -> Left $ MathError "flawed square root operation"

eval vars fs (ToString e) = do
    case eval vars fs e of
        Right (StrVal s) -> Right $ StrVal s
        Right (IntVal i) -> Right $ StrVal (show i)
        Right (FloatVal f) -> Right $ StrVal (show f)
        Left _ -> Left (ValueError "cannot convert to string")

eval vars fs (ToInt e) = do
    value <- eval vars fs e
    case value of
        StrVal s -> case readMaybe s of 
                        Just x -> Right (IntVal x)
                        Nothing -> Left (ValueError "cannot convert to int")
        IntVal i -> Right (IntVal i)
        FloatVal f -> Right (IntVal (truncate f))

eval vars fs (ToFloat e) = do
    value <- eval vars fs e
    case value of
        StrVal s -> case readMaybe s of
                        Just x -> Right (FloatVal x)
                        Nothing -> Left (ValueError "cannot convert to float")
        IntVal i -> Right (FloatVal (fromIntegral i))
        FloatVal f -> Right (FloatVal f)

-- NEED TO FIX THIS
eval vars fs (CallUserFunc name args) = do
    (UserFunc fargs stmts retExpr) <- case value name fs of
        Left _ -> Left $ ValueError ("No such function: " ++ name)
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

intVal :: Value -> Maybe Int
intVal (IntVal i) = Just i
intVal _ = Nothing

strVal :: Value -> Maybe String
strVal (StrVal s) = Just s
strVal _ = Nothing

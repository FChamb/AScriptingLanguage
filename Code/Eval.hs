module Eval where

import Text.Read
import Data.Maybe
import Data.Fixed (divMod') -- For divMod'

import Expr
-- import Struct.BinaryTree

eval :: Vars -> -- Variable name to value mapping
        Funcs ->
        Expr -> -- Expression to evaluate
        Maybe Value -- Result (if no errors such as missing variables)
eval vars fs (Val x) = Just x -- for values, just give the value directly
eval vars fs (Var name) = lookup name vars -- for values, just give the value directly
eval vars fs (Add x y) = do
    xVal <- eval vars fs x
    yVal <- eval vars fs y
    case (xVal, yVal) of
        (IntVal intA, IntVal intB) -> return $ IntVal (intA + intB)
        (FloatVal fltA, FloatVal fltB) -> return $ FloatVal (fltA + fltB)
        (IntVal int, FloatVal flt) -> return $ FloatVal (fromIntegral int + flt)
        (FloatVal flt, IntVal int) -> return $ FloatVal (flt + fromIntegral int)
        _ -> Nothing

eval vars fs (Sub x y) = do
    xVal <- eval vars fs x
    yVal <- eval vars fs y
    case (xVal, yVal) of
        (IntVal intA, IntVal intB) -> return $ IntVal (intA - intB)
        (FloatVal fltA, FloatVal fltB) -> return $ FloatVal (fltA - fltB)
        (IntVal int, FloatVal flt) -> return $ FloatVal (fromIntegral int - flt)
        (FloatVal flt, IntVal int) -> return $ FloatVal (flt - fromIntegral int)
        _ -> Nothing

eval vars fs (Mul x y) = do
    xVal <- eval vars fs x
    yVal <- eval vars fs y
    case (xVal, yVal) of
        (IntVal intA, IntVal intB) -> return $ IntVal (intA * intB)
        (FloatVal fltA, FloatVal fltB) -> return $ FloatVal (fltA * fltB)
        (IntVal int, FloatVal flt) -> return $ FloatVal (fromIntegral int * flt)
        (FloatVal flt, IntVal int) -> return $ FloatVal (flt * fromIntegral int)
        _ -> Nothing

eval vars fs (Div x y) = do
    xVal <- eval vars fs x
    yVal <- eval vars fs y
    case (xVal, yVal) of
        (_, IntVal 0) -> Nothing
        (_, FloatVal 0.0) -> Nothing
        (IntVal intA, IntVal intB) -> return $ if (intA `mod` intB == 0)
                                                  then IntVal (intA `div` intB)
                                                  else FloatVal (fromIntegral intA / fromIntegral intB)
        (FloatVal fltA, FloatVal fltB) -> return $ FloatVal (fltA / fltB)
        (IntVal int, FloatVal flt) -> return $ FloatVal (fromIntegral int / flt)
        (FloatVal flt, IntVal int) -> return $ FloatVal (flt / fromIntegral int)
        _ -> Nothing

eval vars fs (Concat a b) = do
    aStr <- eval vars fs a >>= strVal
    bStr <- eval vars fs b >>= strVal
    return (StrVal (aStr ++ bStr))

eval vars fs (Abs a) = do
    val <- eval vars fs a
    case val of
        IntVal int -> return $ IntVal (abs int)
        FloatVal flt -> return $ FloatVal (abs flt)
        _ -> Nothing

eval vars fs (Mod x y) = do
    dividend <- eval vars fs x
    divisor <- eval vars fs y
    case (dividend, divisor) of
        (_, IntVal 0) -> Nothing
        (_, FloatVal 0.0) -> Nothing
        (IntVal intA, IntVal intB) -> return $ IntVal (intA `mod` intB)
        (FloatVal fltA, FloatVal fltB) -> return $ FloatVal (snd (divMod' fltA fltB))
        (IntVal int, FloatVal flt) -> return $ FloatVal (snd (divMod' (fromIntegral int) flt))
        (FloatVal flt, IntVal int) -> return $ FloatVal (snd (divMod' flt (fromIntegral int)))
        _ -> Nothing

eval vars fs (Pow x y) = do
    base <- eval vars fs x
    exp <- eval vars fs y
    case (base, exp) of
        (IntVal intA, IntVal intB) -> return $ IntVal (intA ^ intB)
        (FloatVal fltA, FloatVal fltB) -> return $ FloatVal (fltA ** fltB)
        (IntVal int, FloatVal flt) -> return $ FloatVal (fromIntegral int ** flt)
        (FloatVal flt, IntVal int) -> return $ FloatVal (flt ** fromIntegral int)

eval vars fs (Sqrt a) = do
    val <- eval vars fs a
    case val of
        IntVal int -> return $ FloatVal (sqrt $ fromIntegral int)
        FloatVal flt -> return $ FloatVal (sqrt flt)
        _ -> Nothing

eval vars fs (ToString e) = do
    value <- eval vars fs e
    return (case value of
        StrVal s -> StrVal s
        IntVal i -> StrVal (show i))

eval vars fs (ToInt e) = do
    value <- eval vars fs e
    case value of
        StrVal s -> readMaybe s >>= Just . IntVal
        IntVal i -> return (IntVal i)
        FloatVal f -> return $ IntVal (truncate f)

eval vars fs (ToFloat e) = do
    value <- eval vars fs e
    case value of
        StrVal s -> readMaybe s >>= Just . FloatVal
        FloatVal f -> return (FloatVal f)

eval vars fs (CallUserFunc name args) = do
    (UserFunc fargs stmts retExpr) <- lookup name fs
    if length args == length fargs
       then do evaledArgs <- sequence $ map (eval vars fs) args
               let fState = zip fargs evaledArgs
               endState <- foldl (evalStmt) (Just fState) stmts
               eval endState fs retExpr
       else Nothing
    where
        evalStmt :: Maybe [(Name, Value)] -> FuncStatement -> Maybe [(Name, Value)]
        evalStmt vars (FuncSetVar n e) = vars >>= updateEval n e
        updateEval :: Name -> Expr -> [(Name, Value)] -> Maybe [(Name, Value)]
        updateEval n e vs = eval vs fs e >>= \x -> Just $ updateVars n x vs

intVal :: Value -> Maybe Int
intVal (IntVal i) = Just i
intVal _ = Nothing

strVal :: Value -> Maybe String
strVal (StrVal s) = Just s
strVal _ = Nothing

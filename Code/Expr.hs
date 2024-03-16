module Expr where

import Data.Fixed -- For divMod'
import Text.Read

import Parsing

type Name = String

data Value = IntVal Int | FloatVal Float | StrVal String
    deriving (Eq)

instance Show Value where
    show (IntVal i) = show i
    show (FloatVal f) = show f
    show (StrVal s) = s

-- At first, 'Expr' contains only addition, conversion to strings, and integer
-- values. You will need to add other operations, and variables
data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Abs Expr
          | Mod Expr Expr
          | Pow Expr Expr 
          | Sqrt Expr
          | ToString Expr
          | ToInt Expr
          | ToFloat Expr
          | Concat Expr Expr
          | Val Value
          | Var Name
  deriving (Show, Eq)

-- These are the REPL commands
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
             | InputSet Name -- Prompt for input and store into variable
             | File Name     -- Prompt for loading a file to run
             | Repeat Int    -- Prompt for repeating a command
             | Quit          -- Prompt for quiting program
             | Help          -- Prompt for showing helpful options
  deriving Show

eval :: [(Name, Value)] -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Maybe Value -- Result (if no errors such as missing variables)
eval vars (Val x) = Just x -- for values, just give the value directly
eval vars (Var name) = lookup name vars -- for values, just give the value directly
eval vars (Add x y) = do
    xVal <- eval vars x
    yVal <- eval vars y
    case (xVal, yVal) of
        (IntVal intA, IntVal intB) -> return $ IntVal (intA + intB)
        (FloatVal fltA, FloatVal fltB) -> return $ FloatVal (fltA + fltB)
        (IntVal int, FloatVal flt) -> return $ FloatVal (fromIntegral int + flt)
        (FloatVal flt, IntVal int) -> return $ FloatVal (flt + fromIntegral int)
        _ -> Nothing

eval vars (Sub x y) = do
    xVal <- eval vars x
    yVal <- eval vars y
    case (xVal, yVal) of
        (IntVal intA, IntVal intB) -> return $ IntVal (intA - intB)
        (FloatVal fltA, FloatVal fltB) -> return $ FloatVal (fltA - fltB)
        (IntVal int, FloatVal flt) -> return $ FloatVal (fromIntegral int - flt)
        (FloatVal flt, IntVal int) -> return $ FloatVal (flt - fromIntegral int)
        _ -> Nothing

eval vars (Mul x y) = do
    xVal <- eval vars x
    yVal <- eval vars y
    case (xVal, yVal) of
        (IntVal intA, IntVal intB) -> return $ IntVal (intA * intB)
        (FloatVal fltA, FloatVal fltB) -> return $ FloatVal (fltA * fltB)
        (IntVal int, FloatVal flt) -> return $ FloatVal (fromIntegral int * flt)
        (FloatVal flt, IntVal int) -> return $ FloatVal (flt * fromIntegral int)
        _ -> Nothing

eval vars (Div x y) = do
    xVal <- eval vars x
    yVal <- eval vars y
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

eval vars (Concat a b) = do
    aStr <- eval vars a >>= strVal
    bStr <- eval vars b >>= strVal
    return (StrVal (aStr ++ bStr))

eval vars (Abs a) = do
    val <- eval vars a
    case val of
        IntVal int -> return $ IntVal (abs int)
        FloatVal flt -> return $ FloatVal (abs flt)
        _ -> Nothing

eval vars (Mod x y) = do
    dividend <- eval vars x
    divisor <- eval vars y
    case (dividend, divisor) of
        (_, IntVal 0) -> Nothing
        (_, FloatVal 0.0) -> Nothing
        (IntVal intA, IntVal intB) -> return $ IntVal (intA `mod` intB)
        (FloatVal fltA, FloatVal fltB) -> return $ FloatVal (snd (divMod' fltA fltB))
        (IntVal int, FloatVal flt) -> return $ FloatVal (snd (divMod' (fromIntegral int) flt))
        (FloatVal flt, IntVal int) -> return $ FloatVal (snd (divMod' flt (fromIntegral int)))
        _ -> Nothing

eval vars (Pow x y) = do
    base <- eval vars x
    exp <- eval vars y
    case (base, exp) of
        (IntVal intA, IntVal intB) -> return $ IntVal (intA ^ intB)
        (FloatVal fltA, FloatVal fltB) -> return $ FloatVal (fltA ** fltB)
        (IntVal int, FloatVal flt) -> return $ FloatVal (fromIntegral int ** flt)
        (FloatVal flt, IntVal int) -> return $ FloatVal (flt ** fromIntegral int)

eval vars (Sqrt a) = do
    val <- eval vars a
    case val of
        IntVal int -> return $ FloatVal (sqrt $ fromIntegral int)
        FloatVal flt -> return $ FloatVal (sqrt flt)
        _ -> Nothing

eval vars (ToString e) = do
    value <- eval vars e
    return (case value of
        StrVal s -> StrVal s
        IntVal i -> StrVal (show i))

eval vars (ToInt e) = do
    value <- eval vars e
    case value of
        StrVal s -> readMaybe s >>= Just . IntVal
        IntVal i -> return (IntVal i)
        FloatVal f -> return $ IntVal (truncate f)

eval vars (ToFloat e) = do
    value <- eval vars e
    case value of
        StrVal s -> readMaybe s >>= Just . FloatVal
        FloatVal f -> return (FloatVal f)

intVal :: Value -> Maybe Int
intVal (IntVal i) = Just i
intVal _ = Nothing

strVal :: Value -> Maybe String
strVal (StrVal s) = Just s
strVal _ = Nothing

{-
 - Context Free Grammar:
 -
 - COMMAND -> VAR = EXPR | VAR = input | print EXPR
 -
 - EXPR -> FACTOR
 -        | TERM + EXPR
 -        | TERM - EXPR
 -        | TERM ++ EXPR
 -
 -
 - TERM -> VAR | VAL | toString(EXPR) | toInt(EXPR) | abs(EXPR) | pow(EXPR,EXPR) | (EXPR)
 -
 - # MUL, DIV, Modulo
 - FACTOR -> TERM
 -     | TERM * TERM
 -     | TERM / TERM
 -     | TERM mod TERM
 -
 - VAL = STRING | INT | FLOAT
 -}

pCommand :: Parser Command
pCommand = do t <- identifier
              symbol "="
              do symbol "input"
                 return (InputSet t)
               ||| do e <- pExpr
                      return (Set t e)
           ||| do string "print"
                  space
                  e <- pExpr
                  return (Print e)
           ||| do string "quit"
                  return Quit
           ||| do string ":f"
                  f <- fileName
                  return (File f)
           ||| do string ":h"
                  return Help


-- Lowest priority operations go here
pExpr :: Parser Expr
pExpr = do t <- pFactor
           do symbol "+"
              e <- pExpr
              return (Add t e)
            ||| do symbol "-"
                   e <- pExpr
                   return (Sub t e)
                 ||| do symbol "++"
                        e <- pExpr
                        return (Concat t e)
                    ||| return t

-- Higher priority operations go here (e.g. multiply, divide)
pFactor :: Parser Expr
pFactor = do f <- pTerm
             do symbol "*"
                t <- pTerm
                return (Mul f t)
                ||| do symbol "/"
                       t <- pTerm
                       return (Div f t)
                       ||| do symbol "%"
                              t <- pFactor
                              return (Mod f t)
                              ||| return f

-- Must be value, variable or something with brackets
pTerm :: Parser Expr
pTerm = do v <- pValue
           return (Val v)
           ||| do symbol "toString"
                  string "("
                  e <- pExpr
                  string ")"
                  return (ToString e)
                  ||| do symbol "toInt"
                         string "("
                         e <- pExpr
                         string ")"
                         return (ToString e)
                       ||| do symbol "toInt"
                              string "("
                              e <- pExpr
                              string ")"
                              return (ToInt e)
                           ||| do symbol "abs"
                                  string "("
                                  e <- pExpr
                                  string ")"
                                  return (Abs e)
                               ||| do symbol "pow"
                                      symbol "("
                                      t <- pExpr
                                      symbol ","
                                      u <- pExpr
                                      symbol ")"
                                      return (Pow t u)
                                 ||| do symbol "sqrt"
                                        string "("
                                        e <- pExpr
                                        string ")"
                                        return (Sqrt e)
                                    ||| do symbol "("
                                           e <- pExpr
                                           symbol ")"
                                           return e
                                           -- Needs to be last so it doesn't try to consume functions
                                        ||| do v <- identifier
                                               return (Var v)

-- Parsing of values
pValue :: Parser Value
pValue = do f <- float
            return (FloatVal f)
            ||| do i <- integer
                   return (IntVal i)
                   ||| do s <- quotedString
                          return (StrVal s)

quotedString :: Parser String
quotedString = do
        char '"'
        s <- many notQuote
        char '"'
        return s

notQuote = sat (\x -> x /= '"')

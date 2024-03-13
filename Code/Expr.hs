module Expr where

import Text.Read

import Parsing

type Name = String

data Value = IntVal Int | StrVal String

instance Show Value where
    show (IntVal i) = show i
    show (StrVal s) = s

-- At first, 'Expr' contains only addition, conversion to strings, and integer
-- values. You will need to add other operations, and variables
data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Abs Expr
          | ToString Expr
          | ToInt Expr
          | Concat Expr Expr
          | Val Value
          | Var Name
  deriving Show

-- These are the REPL commands
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
             | InputSet Name -- Prompt for input and store into variable
  deriving Show

eval :: [(Name, Value)] -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Maybe Value -- Result (if no errors such as missing variables)
eval vars (Val x) = Just x -- for values, just give the value directly
eval vars (Var name) = lookup name vars -- for values, just give the value directly
eval vars (Add x y) = do
    xInt <- eval vars x >>= intVal
    yInt <- eval vars y >>= intVal
    return (IntVal (xInt + yInt))

eval vars (Sub x y) = do
    xInt <- eval vars x >>= intVal
    yInt <- eval vars y >>= intVal
    return (IntVal (xInt - yInt))

eval vars (Mul x y) = do
    xInt <- eval vars x >>= intVal
    yInt <- eval vars y >>= intVal
    return (IntVal (xInt * yInt))

eval vars (Div x y) = do
    xInt <- eval vars x >>= intVal
    yInt <- eval vars y >>= intVal
    return (IntVal (xInt `div` yInt))

eval vars (Concat a b) = do
    aStr <- eval vars a >>= strVal
    bStr <- eval vars b >>= strVal
    return (StrVal (aStr ++ bStr))

eval vars (Abs a) = do
    val <- eval vars a >>= intVal
    return (IntVal (abs val))

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

intVal :: Value -> Maybe Int
intVal (IntVal i) = Just i
intVal _ = Nothing

strVal :: Value -> Maybe String
strVal (StrVal s) = Just s
strVal _ = Nothing

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

pExpr :: Parser Expr
pExpr = do t <- pTerm
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

pFactor :: Parser Expr
pFactor = do i <- integer
             return (Val (IntVal (i)))
           ||| do s <- quotedString
                  return (Val (StrVal s))
                  ||| do symbol "toString"
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
                               ||| do symbol "("
                                      e <- pExpr
                                      symbol ")"
                                      return e
                                   ||| do v <- identifier
                                          return (Var v)

pTerm :: Parser Expr
pTerm = do f <- pFactor
           do symbol "*"
              t <- pTerm
              return (Mul f t)
            ||| do symbol "/"
                   t <- pTerm
                   return (Div f t)
                 ||| return f

quotedString :: Parser String
quotedString = do
        char '"'
        s <- many notQuote
        char '"'
        return s

notQuote = sat (\x -> x /= '"')

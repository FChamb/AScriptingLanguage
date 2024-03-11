module Expr where

import Parsing

type Name = String

data Value = IntVal Int | StrVal String
    deriving Show

-- At first, 'Expr' contains only addition, conversion to strings, and integer
-- values. You will need to add other operations, and variables
data Expr = Add Expr Expr
          | ToString Expr
          | Val Value
  deriving Show

-- These are the REPL commands
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
  deriving Show

eval :: [(Name, Value)] -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Maybe Value -- Result (if no errors such as missing variables)
eval vars (Val x) = Just x -- for values, just give the value directly
eval vars (Add x y) = do -- return an error (because it's not implemented yet!)
    xInt <- eval vars x >>= intVal
    yInt <- eval vars y >>= intVal
    return (IntVal (xInt + yInt))

eval vars (ToString e) = do
    value <- eval vars e
    return (case value of
        StrVal s -> StrVal s
        IntVal i -> StrVal (show i))

intVal :: Value -> Maybe Int
intVal (IntVal i) = Just i
intVal _ = Nothing

digitToInt :: Char -> Int
digitToInt x = fromEnum x - fromEnum '0'

pCommand :: Parser Command
pCommand = do t <- letter
              char '='
              e <- pExpr
              return (Set [t] e)
            ||| do string "print"
                   space
                   e <- pExpr
                   return (Print e)

pExpr :: Parser Expr
pExpr = do t <- pTerm
           do char '+'
              e <- pExpr
              return (Add t e)
            ||| do char '-'
                   e <- pExpr
                   error "Subtraction not yet implemented!" 
                 ||| return t

pFactor :: Parser Expr
pFactor = do d <- digit
             return (Val (IntVal (digitToInt d)))
           ||| do v <- letter
                  error "Variables not yet implemented" 
                ||| do char '('
                       e <- pExpr
                       char ')'
                       return e

pTerm :: Parser Expr
pTerm = do f <- pFactor
           do char '*'
              t <- pTerm
              error "Multiplication not yet implemented" 
            ||| do char '/'
                   t <- pTerm
                   error "Division not yet implemented" 
                 ||| return f

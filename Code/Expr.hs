module Expr where

import Data.Fixed -- For divMod'
import Text.Read

import Parsing

{-- TODO: Put in State.hs once circular dependency broken --}
type Name = String

data Value = IntVal Int | FloatVal Float | StrVal String
    deriving (Eq)

instance Show Value where
    show (IntVal i) = show i
    show (FloatVal f) = show f
    show (StrVal s) = s
type Vars = [(Name, Value)]
type Funcs = [(Name, UserFunc)]

data LState = LState { vars :: Vars,
                       funcs :: Funcs }

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> Value -> Vars -> Vars
updateVars name val vars = (name,val):(dropVar name vars)


-- Return a new set of variables with the given name removed
dropVar :: Name -> Vars -> Vars
dropVar name vars = filter (\(n, v) -> n /= name) vars

updateFunc :: Name -> UserFunc -> Funcs -> Funcs
updateFunc name func funcs = (name, func):removed
    where removed = filter (\(n, _) -> n /= name) funcs

{- End of stuff that should be moved into State.hs -}

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
          | CallUserFunc Name [Expr] -- User defined function, with variable number of arguments
          | Val Value
          | Var Name
  deriving (Show, Eq)

-- These are the REPL commands
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
             | InputSet Name -- Prompt for input and store into variable
             | File Name     -- Prompt for loading a file to run
             | Repeat Int    -- Prompt for repeating a command
             | DefUserFunc Name UserFunc -- Define a function
             | Quit          -- Prompt for quiting program
             | Help          -- Prompt for showing helpful options
  deriving Show

-- Arguments, Statements, Return value
data UserFunc = UserFunc [Name] [FuncStatement] Expr
    deriving (Show)
data FuncStatement = FuncSetVar Name Expr -- TODO: support some kind of if too?
    deriving (Show)


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
           ||| do pFunc
           ||| do string ":f"
                  f <- fileName
                  return (File f)
           ||| do string ":h"
                  return Help

pFunc :: Parser Command
pFunc = do symbol "def" -- Define a function
           fName <- identifier
           symbol "("
           args <- functionArgsDef
           symbol ")"
           symbol "{"
           stmts <- many pFuncStatement
           retExpr <- pExpr
           symbol "}"
           return $ DefUserFunc fName (UserFunc args stmts retExpr)

pFuncStatement :: Parser FuncStatement
pFuncStatement = do
    do t <- identifier
       symbol "="
       e <- pExpr
       symbol ";"
       return $ FuncSetVar t e

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
           ||| do n <- identifier
                  symbol "("
                  args <- functionArgsCall
                  symbol ")"
                  return (CallUserFunc n args)
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

withLeadingComma :: Parser a -> Parser a
withLeadingComma p = do symbol ","
                        p

-- Parse a list of function arguments
functionArgsDef :: Parser [Name]
functionArgsDef = do firstArg <- identifier
                     args <- many (withLeadingComma identifier)
                     return $ firstArg:args
                  ||| return []

functionArgsCall :: Parser [Expr]
functionArgsCall = do firstArg <- pExpr
                      args <- many (withLeadingComma pExpr)
                      return $ firstArg:args
                   ||| return []

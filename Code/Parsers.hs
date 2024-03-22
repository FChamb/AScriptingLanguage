module Parsers where

import Data.List (nub)
import Data.Either (rights)

import Parsing
import Expr
import Error

{-
 - Context Free Grammar:
 -
 - COMMAND -> VAR = EXPR | VAR = input | print EXPR | def FUNCTION | if EXPR { Block }
 -
 - EXPR -> MATHEXPR == MATHEXPR
 -       | MATHEXPR != MATHEXPR
 -       | MATHEXPR < MATHEXPR
 -       | MATHEXPR > MATHEXPR
 -       | MATHEXPR
 -
 - MATHEXPR -> FACTOR
 -        | TERM + EXPR
 -        | TERM - EXPR
 -        | TERM ++ EXPR
 -        | if EXPR then EXPR else EXPR
 -
 -
 - TERM -> VAR | VAL | toString(EXPR) | toInt(EXPR) | abs(EXPR) | pow(EXPR,EXPR) | (EXPR)
 -
 - # MUL, DIV, Modulo
 - FACTOR -> TERM
 -     | TERM * FACTOR
 -     | TERM / FACTOR
 -     | TERM mod TERM
 -
 - VAL = STRING | INT | FLOAT
 -}

pCommand :: Parser (Either Error Command)
pCommand = do t <- identifier
              symbol "="
              do symbol "input"
                 return (Right (InputSet t))
               ||| do e <- pExpr
                      return (Right (Set t e))
           ||| do string "print"
                  space
                  e <- pExpr
                  return (Right (Print e))
           ||| do string "quit"
                  return (Right Quit)
           ||| do pFunc
           ||| do string "while"
                  symbol "(" 
                  condition <- pExpr
                  symbol ")" 
                  block <- pBlock
                  return (Right (While condition block))
           ||| do string "for"
                  symbol "("
                  condition <- pExpr
                  symbol ";"
                  operation <- pCommand
                  symbol ")" 
                  block <- pBlock
                  case operation of 
                       Right op -> return (Right (For condition op block))
                       Left err -> return $ Left err
           ||| do string "repeat"
                  space
                  i <- integer
                  space
                  block <- pBlock
                  return (Right (Repeat i [block]))
           ||| do string "if"
                  space
                  condition <- pExpr
                  space
                  string "then"
                  space
                  thenBlock <- pCommand
                  space
                  string "else"
                  space
                  elseBlock <- pCommand
                  case thenBlock of
                      Right thenCmd ->
                          case elseBlock of
                              Right elseCmd -> return (Right (If condition thenCmd elseCmd))
                              Left err -> return (Left err)
                      Left err -> return (Left err)
           ||| do string ":load"
                  f <- fileName
                  return (Right (LoadFile f))
           ||| do string ":help"
                  return (Right Help)
           ||| return (Left (ParseError "Invalid command"))


pBlock :: Parser Command
pBlock = do
    symbol "{"
    space
    c <- pCommand `sepBy1'` symbol ";"
    space
    symbol "}"
    return $ Block (rights c)

pFunc :: Parser (Either Error Command)
pFunc = do symbol "def" -- Define a function
           fName <- identifier
           symbol "("
           args <- functionArgsDef
           symbol ")"
           symbol "{"
           stmts <- many pFuncStatement
           retExpr <- pExpr
           symbol "}"
           if nub args == args
              then return $ Right (DefUserFunc fName (UserFunc args stmts retExpr))
              else return (Left (ParseError "Argument names must be unique"))

pFuncStatement :: Parser FuncStatement
pFuncStatement = do
    do t <- identifier
       symbol "="
       e <- pExpr
       symbol ";"
       return $ FuncSetVar t e

-- Conditionals (==), (!=), lowest priority / evaluated last
pExpr :: Parser Expr
pExpr = do a <- pMathExpr
           do symbol "=="
              b <- pMathExpr
              return (IsEq a b)
              ||| do symbol "!="
                     b <- pMathExpr
                     return (NotEq a b)
              ||| do symbol "<"
                     b <- pMathExpr
                     return (LessThan a b)
              ||| do symbol ">"
                     b <- pMathExpr
                     return (GreaterThan a b)
              ||| return a

-- Next lowest priority operations go here
pMathExpr :: Parser Expr
pMathExpr = do t <- pFactor
               do symbol "+"
                  e <- pMathExpr
                  return (Add t e)
                  ||| do symbol "-"
                         e <- pMathExpr
                         return (Sub t e)
                  ||| do symbol "++"
                         e <- pMathExpr
                         return (Concat t e)
                  ||| return t

-- Higher priority operations go here (e.g. multiply, divide)
pFactor :: Parser Expr
pFactor = do f <- pTerm
             do symbol "*"
                t <- pFactor
                return (Mul f t)
                ||| do symbol "/"
                       t <- pFactor
                       return (Div f t)
                ||| do symbol "%"
                       t <- pTerm
                       return (Mod f t)
                ||| return f

-- Must be value, variable or something with brackets
pTerm :: Parser Expr
pTerm = do v <- pValue
           return (Val v)
           ||| do symbol "toString"
                  symbol "("
                  e <- pExpr
                  symbol ")"
                  return (ToString e)
           ||| do symbol "toInt"
                  symbol "("
                  e <- pExpr
                  symbol ")"
                  return (ToInt e)
           ||| do symbol "abs"
                  symbol "("
                  e <- pExpr
                  symbol ")"
                  return (Abs e)
           ||| do symbol "pow"
                  symbol "("
                  t <- pExpr
                  symbol ","
                  u <- pExpr
                  symbol ")"
                  return (Pow t u)
           ||| do symbol "sqrt"
                  symbol "("
                  e <- pExpr
                  symbol ")"
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

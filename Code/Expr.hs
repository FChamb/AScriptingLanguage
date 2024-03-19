module Expr where

import BinaryTree

type Name = String

data Value = IntVal Int | FloatVal Float | StrVal String
    deriving (Eq)

instance Show Value where
    show (IntVal i) = show i
    show (FloatVal f) = show f
    show (StrVal s) = s

type Vars = Tree (Name, Value)

{-
 - Functions
 -
 - Functions work by having a series a statements (terminted by a semi colon)
 - followed by a final "return" expression, which is evaluated and given
 - as the result of the function as a whole
 -}
data UserFunc = UserFunc [Name] [FuncStatement] Expr
    deriving (Show)
data FuncStatement = FuncSetVar Name Expr -- TODO: support some kind of if too?
    deriving (Show)
type Funcs = [(Name, UserFunc)]

data State = State {vars :: Tree (Name, Value), funcs :: Funcs, history :: [Command]}

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> Value -> State -> State
updateVars name val st = st {vars = insert (name, val) (vars st)}

updateFunc :: Name -> UserFunc -> Funcs -> Funcs
updateFunc name func funcs = (name, func):removed
    where removed = filter (\(n, _) -> n /= name) funcs
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

-- Error data type
data Error = ParseError String | MathError String | ValueError String
    deriving (Eq)

instance Show Error where
    show (ParseError i) = "ParseError: " ++ i
    show (MathError f) =  "MathError: " ++ f
    show (ValueError s) = "ValueError: " ++ s

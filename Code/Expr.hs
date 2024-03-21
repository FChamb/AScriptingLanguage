module Expr where

import BinaryTree
import Control.Monad.State

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
type Funcs = Tree (Name, UserFunc)

data Env = Env {vars :: Vars, funcs :: Funcs, history :: [Command]}
type Eval a = StateT Env (Either Error) a
--data State = State {vars :: Tree (Name, Value), funcs :: Funcs, history :: [Command]}

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> Value -> Eval ()
updateVars name val = do
    env <- get
    let newVars = insert (name, val) (vars env)
    put $ env { vars = newVars }

{- An Expression is a tree-like data type that can be evaluated to a Value,
 - without any IO. It only consists of mathematical statements -}
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
          | If Expr Expr
  deriving (Show, Eq)

-- These are the REPL commands
data Command = Set Name Expr             -- assign an expression to a variable name
             | Print Expr                -- evaluate an expression and print the result
             | InputSet Name             -- Prompt for input and store into variable
             | File Name                 -- Prompt for loading a file to run
             | Repeat Int Command        -- Prompt for repeating a command
             | DefUserFunc Name UserFunc -- Define a function
             | Quit                      -- Prompt for quiting program
             | Help                      -- Prompt for showing helpful options
  deriving Show

-- Error data type
data Error = ParseError String | MathError String | ValueError String
    deriving (Eq)

instance Show Error where
    show (ParseError i) = "ParseError: " ++ i
    show (MathError f) =  "MathError: " ++ f
    show (ValueError s) = "ValueError: " ++ s

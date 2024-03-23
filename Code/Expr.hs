module Expr where

import BinaryTree
import Control.Monad.State
import Error

type Name = String -- type definition for variable names

data Value = IntVal Int | FloatVal Float | StrVal String | BoolVal Bool
    deriving (Eq)

instance Show Value where
    show (IntVal i) = show i
    show (FloatVal f) = show f
    show (StrVal s) = s
    show (BoolVal b) = show b

type Vars = Tree (Name, Value) -- variables represented as binary tree

{- Given a variable name and a value, updates state with that name 
 - and value added. If it already exists, overwrites old value -}
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
          | IsEq Expr Expr           -- Boolean operations
          | NotEq Expr Expr
          | LessThan Expr Expr
          | GreaterThan Expr Expr
          | Val Value
          | Var Name
  deriving (Show, Eq)

-- These are the REPL commands
data Command = Set Name Expr             -- assign an expression to a variable name
             | Print Expr                -- evaluate an expression and print the result
             | InputSet Name             -- Prompt for input and store into variable
             | LoadFile FilePath         -- Prompt for loading a file to run
             | Repeat Int [Command]      -- Prompt for repeating a command
             | Block [Command]           -- Representing a block command
             | DefUserFunc Name UserFunc -- Define a function
             | If Expr Command Command   -- If then else construct
             | While Expr Command        -- While loop construct
             | For Expr Command Command  -- For loop construct
             | Quit                      -- Prompt for quiting program
             | Help                      -- Prompt for showing helpful options
  deriving Show

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

{-
 - StateT
 -
 - Implementation of StateT with first defines a data type, Env. Then a type
 - Eval is constructed using the Env data and StateT syntax. Commented out code
 - below represents the old data State type.
 -}
data Env = Env {vars :: Vars, funcs :: Funcs}
type Eval a = StateT Env (Either Error) a
--data State = State {vars :: Tree (Name, Value), funcs :: Funcs}
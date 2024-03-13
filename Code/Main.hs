module Main where

import System.Console.Haskeline

import Parsing
import Expr
import REPL

main :: IO ()
main = runInputT defaultSettings (repl initLState)

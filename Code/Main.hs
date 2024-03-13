module Main where

import System.Console.Haskeline

import Parsing
import Expr
import REPL
import Test

main :: IO ()
main = runInputT defaultSettings (repl initLState)

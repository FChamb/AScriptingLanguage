module Main where

import System.Console.Haskeline
import System.Environment

import Parsing
import Expr
import REPL

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do loadFile file initState
        _ -> runInputT defaultSettings (repl initState)

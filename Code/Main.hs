module Main where

import System.Console.Haskeline
import System.Environment
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.Except

import Parsing
import Expr
import REPL

{- Main
 -
 - Main entry point into program. Takes command line arguments
 - as an input for possibly loading a file to run. If nothing
 - is provided, REPL loop starts at default state.
-}
main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do loadFile file
        _ -> runREPL initState

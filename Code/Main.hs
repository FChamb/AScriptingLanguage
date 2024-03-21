module Main where

import System.Console.Haskeline
import System.Environment
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.Except

import Parsing
import Expr
import REPL

main :: IO ()
main = do
    args <- getArgs
    case args of
        --[file] -> do loadFile file initState
        _ -> runREPL initState

module REPL where

import System.Console.Haskeline
import Control.Monad.IO.Class
import System.Directory

import Expr
import Parsing
import Parsers
import Eval


initLState :: LState
initLState = LState [] []


{-process :: LState -> Command -> IO ()
process st (Set var e) = do let st' = case (eval e) of
                                (Just x) -> updateVars var x (vars LState)
                                Nothing -> do
                                    putStrLn("Bad.")
                                    st
                            repl st'
-}

process :: LState -> Command -> InputT IO ()
process st (Set var e) = do
    let evaled = eval (vars st) (funcs st) e
    let st' = case evaled of
                Just x -> st{vars= (updateVars var x (vars st))}
                Nothing -> st -- print bad ????
    repl st'

process st (InputSet var) = do
    input <- getInputLine ""
    case input of
        Nothing -> do outputStrLn "EOF, cancelling input"
                      repl st
        Just inp -> do let newVars = updateVars var (StrVal inp) (vars st)
                       let st' = st { vars = newVars }
                       repl st'

process st (Print e) = do
    case eval (vars st) (funcs st) e of
        Just evaled -> outputStrLn (show evaled)
        Nothing -> return ()
    -- Print the result of evaluation
    repl st

process st (File f) = do
    x <- liftIO(doesFileExist f)
    if x then do outputStrLn "Loading File..."
                 liftIO(loadFile f st)
    else do outputStrLn ("\"" ++ f ++ "\"" ++ "does not exist!")
            repl st

process st (DefUserFunc name func) = do
    let st' = st { funcs = (updateFunc name func (funcs st))}
    repl st'

process st (Help) = do
    outputStrLn("List of program operations: ")
    outputStrLn("  - a + b {Addition}")
    outputStrLn("  - a - b {Subtraction}")
    outputStrLn("  - a * b {Multiplication}")
    outputStrLn("  - a / b {Division}")
    outputStrLn("  - |a| {Absolute Value}")
    outputStrLn("  - a % b {Modulus}")
    outputStrLn("  - a ^ b {Power}")
    outputStrLn("  - sqrt a {Square Root}")
    outputStrLn("  - a = 1 {Assign variables}")
    outputStrLn("Lost of program commands: ")
    outputStrLn("  - quit {Quit the Program}")
    outputStrLn("  - :f fileName {Load a File}")
    outputStrLn("  - :h {Show Program Commands")
    repl st

process st (Quit) = outputStrLn("Closing")

loadFile :: Name -> LState -> IO ()
loadFile file st = runInputTBehavior (useFile file) defaultSettings (repl st)

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: LState -> InputT IO ()
repl st = do outputStrLn (show (vars st))
             inp <- getInputLine "> "
             let parsed = fmap (parse pCommand) inp
             case parsed of
                  Nothing -> outputStrLn "EOF, goodbye"
                  Just [(cmd, "")] -> -- Must parse entire input
                          process st cmd
                  _ -> do outputStrLn "Parse error"
                          repl st

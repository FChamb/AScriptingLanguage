module REPL where

import Expr
import Parsing

data LState = LState { vars :: [(Name, Value)] }

initLState :: LState
initLState = LState []

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> Value -> [(Name, Value)] -> [(Name, Value)]
updateVars name val vars = (name,val):(dropVar name vars)


-- Return a new set of variables with the given name removed
dropVar :: Name -> [(Name, Value)] -> [(Name, Value)]
dropVar name vars = filter (\(n, v) -> n /= name) vars

{-process :: LState -> Command -> IO ()
process st (Set var e) = do let st' = case (eval e) of
                                (Just x) -> updateVars var x (vars LState)
                                Nothing -> do
                                    putStrLn("Bad.")
                                    st
                            repl st'
-}

process :: LState -> Command -> IO ()
process st (Set var e) = do
    let evaled = eval (vars st) e
    let st' = case evaled of
                Just x -> st{vars= (updateVars var x (vars st))}
                Nothing -> st -- print bad ????
    repl st'

process st (InputSet var) = do
    input <- getLine
    let newVars = updateVars var (StrVal input) (vars st)
    let st' = st { vars = newVars }
    repl st'

process st (Print e) = do
    case eval (vars st) e of
        Just evaled -> print evaled
        Nothing -> return ()
    -- Print the result of evaluation
    repl st

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: LState -> IO ()
repl st = do print (vars st)
             putStr ("> ")
             inp <- getLine
             case parse pCommand inp of
                  [(cmd, "")] -> -- Must parse entire input
                          process st cmd
                  _ -> do putStrLn "Parse error"
                          repl st

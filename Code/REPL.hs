module REPL where

import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad (replicateM_)
import Control.Monad (forM_)
import Control.Monad (when)
import System.Directory
import System.Exit (exitSuccess)
import Data.Either (rights)

import Expr
import Parsing
import Parsers
import Eval
import BinaryTree
import Error

initState :: Env
initState = Env Empty Empty []

type REPL a = StateT Env (ExceptT Error (InputT IO)) a

{-process :: LState -> Command -> IO ()
process st (Set var e) = do let st' = case (eval e) of
                                (Just x) -> updateVars var x (vars LState)
                                Nothing -> do
                                    putStrLn("Bad.")
                                    st
                            repl st'
-}

process :: Command -> Bool -> REPL ()
process (Set var e) continue = do
    st <- get
    let evaled = eval (vars st) (funcs st) e
    case evaled of
        Right x -> modify (\s -> s { vars = insert (var, x) (vars s) })
        Left e -> liftIO $ putStrLn (show e)
    repl continue

process (InputSet var) continue = do
    input <- lift $ lift $ getInputLine ""
    case input of
        Nothing -> liftIO $ putStrLn "EOF, cancelling input"
        Just inp -> do st <- get
                       let newVars = insert (var, StrVal inp) (vars st)
                       modify (\s -> s { vars = newVars })
    repl True

process (Print e) continue = do
    st <- get
    case eval (vars st) (funcs st) e of
        Right evaled -> liftIO $ putStrLn (show evaled)
        Left e -> liftIO $ putStrLn (show e)
    repl continue


process (LoadFile f) continue = do
    liftIO $ putStrLn $ "Loading File..."
    liftIO $ loadFile f

process (Repeat n cmd) continue = do
    st <- get
    forM_ (repeatCmds cmd) (\c -> process c continue)
    --replicateM_ n (sequence_ (map process (repeatCmds cmd) continue))
    put st
    repl False

process (Block cmds) continue = do
    st <- get
    forM_ cmds (\c -> process c continue)
    --replicateM_ (length cmds) (sequence_ (map process cmds continue))
    put st
    repl False

process (DefUserFunc name func) continue = do
    modify (\s -> s { funcs = insert (name, func) (funcs s) })
    repl True

process (If condition thenBlock elseBlock) continue = do
    st <- get
    case eval (vars st) (funcs st) condition of
        Right (IntVal val) -> if val /= 0
                              then process (Block [thenBlock]) continue
                              else process (Block [elseBlock]) continue
        Left e -> liftIO $ putStrLn (show e)
    repl continue

process (Help) continue = do
    liftIO $ putStrLn ("List of program operations: ")
    liftIO $ putStrLn ("  - a + b {Addition}")
    liftIO $ putStrLn ("  - a - b {Subtraction}")
    liftIO $ putStrLn ("  - a * b {Multiplication}")
    liftIO $ putStrLn ("  - a / b {Division}")
    liftIO $ putStrLn ("  - |a| {Absolute Value}")
    liftIO $ putStrLn ("  - a % b {Modulus}")
    liftIO $ putStrLn ("  - a ^ b {Power}")
    liftIO $ putStrLn ("  - sqrt a {Square Root}")
    liftIO $ putStrLn ("  - a = 1 {Assign variables}")
    liftIO $ putStrLn ("List of program commands: ")
    liftIO $ putStrLn ("  - print ... {Print the Command}")
    liftIO $ putStrLn ("  - quit {Quit the Program}")
    liftIO $ putStrLn ("  - :f fileName {Load a File}")
    liftIO $ putStrLn ("  - :h {Show Program Commands")
    repl True

process (Quit) continue = do liftIO $ putStrLn ("Closing")
                             liftIO exitSuccess


loadFile :: Name -> IO ()
loadFile file = do
    let replIO :: REPL ()
        replIO = repl False
    result <- runInputTBehavior (useFile file) defaultSettings (runExceptT (evalStateT (replIO) initState))
    case result of
        Left err -> putStrLn $ "Error: " ++ (show err)
        Right newState -> return ()

remember :: Command -> StateT Env (ExceptT Error IO) ()
remember cmd = do
    st <- get
    put $ st { history = cmd : history st }

repeatCmds :: Command -> [Command]
repeatCmds (Block cmds) = cmds
repeatCmd cmd = [cmd]

runREPL :: Env -> IO ()
runREPL initState = do
    result <- runInputT defaultSettings (runExceptT (evalStateT (repl True) initState))
    case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right _ -> return ()

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: Bool -> REPL ()
repl continue = do
    input <- case continue of
        True -> lift $ lift $ getInputLine "> "
        False -> lift $ lift $ getInputLine ""
    case input of
        Nothing -> case continue of
            True -> liftIO $ putStrLn "EOF, goodbye"
            False -> liftIO $ putStr ""
        Just line -> do
            let parsed = parse pCommand line
            case parsed of
                [(Right cmd, "")] -> process cmd continue
                [(Left err, _)] -> liftIO $ putStrLn (show err)
                [(Right _, rem)] -> liftIO $ putStrLn ("Error: Unconsumed input '" ++ rem ++ "'")
                _ -> liftIO $ putStrLn ("Error: Invalid input")
            repl continue

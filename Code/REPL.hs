module REPL where

import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad (replicateM_)
import Control.Monad (forM_)
import Control.Monad (when)
import Control.Monad (guard)
import System.Directory
import System.Exit (exitSuccess)
import Data.Either (rights)
import System.IO.Error
import Control.Exception

import Expr
import Parsing
import Parsers
import Eval
import BinaryTree
import Error
import Completion

initState :: Env
initState = Env Empty Empty []

--type REPL a = StateT Env (ExceptT Error (InputT IO)) a
type REPL a = ExceptT Error (InputT (StateT Env IO)) a

liftState = lift . lift
liftInput = lift

{-process :: LState -> Command -> IO ()
process st (Set var e) = do let st' = case (eval e) of
                                (Just x) -> updateVars var x (vars LState)
                                Nothing -> do
                                    putStrLn("Bad.")
                                    st
                            repl st'
-}

process :: Command -> REPL ()
process (Set var e) = do
    st <- liftState $ get
    let evaled = eval (vars st) (funcs st) e
    case evaled of
        Right x -> liftState $ modify (\s -> s { vars = insert (var, x) (vars s) })
        Left e -> liftIO $ putStrLn (show e)

process (InputSet var) = do
    input <- liftInput $ getInputLine ""
    case input of
        Nothing -> liftIO $ putStrLn "EOF, cancelling input"
        Just inp -> do st <- liftState $ get
                       let newVars = insert (var, StrVal inp) (vars st)
                       liftState $ modify (\s -> s { vars = newVars })


process (Print e) = do
    st <- liftState $ get
    case eval (vars st) (funcs st) e of
        Right evaled -> liftIO $ putStrLn (show evaled)
        Left e -> liftIO $ putStrLn (show e)

process (LoadFile f) = do
    liftIO $ putStrLn $ "Loading File..."
    liftIO $ loadFile f

process (Repeat n cmds) = do
    st <- liftState $ get
    forM_ [1..n] (\_ -> forM_ cmds (\c -> process c))
    liftState $ put st

process (Block cmds) = do
    st <- liftState $ get
    forM_ cmds (\c -> process c)
    liftState $ put st

process (DefUserFunc name func) = do
    liftState $ modify (\s -> s { funcs = insert (name, func) (funcs s) })


process (If condition thenBlock elseBlock) = do
    st <- liftState $ get
    case eval (vars st) (funcs st) condition of
        Right (BoolVal b) -> if b
                              then process (Block [thenBlock])
                              else process (Block [elseBlock])
        Left e -> liftIO $ putStrLn (show e)

process (Help) = do
    liftIO $ putStrLn ("List of program operations: ")
    liftIO $ putStrLn ("  - a + b {Addition}")
    liftIO $ putStrLn ("  - a - b {Subtraction}")
    liftIO $ putStrLn ("  - a * b {Multiplication}")
    liftIO $ putStrLn ("  - a / b {Division}")
    liftIO $ putStrLn ("  - abs(a) {Absolute Value}")
    liftIO $ putStrLn ("  - a % b {Modulus}")
    liftIO $ putStrLn ("  - pow(a,b) {Power}")
    liftIO $ putStrLn ("  - sqrt(a) {Square Root}")
    liftIO $ putStrLn ("  - a = 1 {Assign variables}")
    liftIO $ putStrLn ("  - If x then y else z {If then else conditionals")
    liftIO $ putStrLn ("List of program commands: ")
    liftIO $ putStrLn ("  - print ... {Print the Command}")
    liftIO $ putStrLn ("  - repeat n {} {Repeat an operation, n times")
    liftIO $ putStrLn ("  - quit {Quit the Program}")
    liftIO $ putStrLn ("  - :load fileName {Load a File}")
    liftIO $ putStrLn ("  - :help {Show Program Commands")

process (Quit) = do liftIO $ putStrLn ("Closing")
                    liftIO exitSuccess

loadFile :: Name -> IO ()
loadFile file = do
    result <- tryJust (guard . isDoesNotExistError) $
              evalStateT (runInputTBehavior (useFile file) defaultSettings (runExceptT (repl False))) initState
    case result of
        Left err -> putStrLn "Error: File not found"
        Right (Left err) -> do putStrLn $ show err
        Right (Right newState) -> return ()

remember :: Command -> StateT Env (ExceptT Error IO) ()
remember cmd = do
    st <- get
    put $ st { history = cmd : history st }

repeatCmds :: Command -> [Command]
repeatCmds (Block cmds) = concatMap repeatCmds cmds
repeatCmd cmd = [cmd]

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: Bool -> REPL ()
repl continue = do
    input <- case continue of
        True -> liftInput $ getInputLine "> "
        False -> liftInput $ getInputLine ""
    case input of
        Nothing -> case continue of
            True -> liftIO $ putStrLn "EOF, goodbye"
            False -> liftIO $ putStr ""
        Just line -> do
            let parsed = parse pCommand line
            case parsed of
                [(Right cmd, "")] -> process cmd
                [(Right cmd, rem)] -> liftIO $ putStrLn ("ParseError: Unconsumed input '" ++ rem ++ "'")
                [(Left err, _)] -> liftIO $ putStrLn (show err)
            repl continue

runREPL :: Env -> IO ()
runREPL initState = do
    result <- evalStateT (runInputT hlSettings (runExceptT (repl True))) initState
    case result of
        Left err -> putStrLn $ show err
        Right _ -> return ()

module REPL where

import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad (replicateM_)
import Control.Monad (forM_)
import Control.Monad (when)
import Control.Monad (guard)
import System.Exit (exitSuccess)
import Data.Either (rights)
import System.IO.Error (isDoesNotExistError)
import Control.Exception (tryJust)
import Data.List (dropWhileEnd)
import Data.Char (isSpace)

import Expr
import Parsing
import Parsers
import Eval
import BinaryTree
import Error
import Completion

-- Default state represented by Env for starting program loop
initState :: Env
initState = Env Empty Empty

--type REPL a = StateT Env (ExceptT Error (InputT IO)) a
type REPL a = ExceptT Error (InputT (StateT Env IO)) a

liftState = lift . lift
liftInput = lift

{- Process Commands
 -
 - Process is a function which takes a user command and provides a REPL
 - type for the haskeline implementation of the program loop. REPL type
 - is defined above. There are various options for every input command
 - to process.
-}
process :: Command -> REPL ()
process (Set var e) = do
    st <- liftState $ get
    let evaled = eval (vars st) (funcs st) e
    case evaled of
        Right x -> liftState $ modify (\s -> s { vars = insert (var, x) (vars s) })
        Left e -> liftIO $ putStrLn (show e)

-- InputSet processes the user provided options to define a new variable
process (InputSet var) = do
    input <- liftInput $ getInputLine ""
    case input of
        Nothing -> liftIO $ putStrLn "EOF, cancelling input"
        Just inp -> do st <- liftState $ get
                       let newVars = insert (var, StrVal inp) (vars st)
                       liftState $ modify (\s -> s { vars = newVars })

-- Print is supported to print the result or output of a argument
process (Print e) = do
    st <- liftState $ get
    case eval (vars st) (funcs st) e of
        Right evaled -> liftIO $ putStrLn (show evaled)
        Left e -> liftIO $ putStrLn (show e)

-- LoadFile processes a provided file input by calling the appropriate function
process (LoadFile f) = do
    liftIO $ putStrLn $ "Loading File..."
    liftIO $ loadFile f

-- Repeat takes the provided number of repetitions and uses monadic action to repeat process a command
process (Repeat n cmds) = do
    st <- liftState $ get
    forM_ [1..n] (\_ -> forM_ cmds (\c -> process c))

-- Block enables functionality for repeat, if then else, while and for loops
process (Block cmds) = do
    st <- liftState $ get
    forM_ cmds (\c -> process c)

-- Define a user specific function
process (DefUserFunc name func) = do
    liftState $ modify (\s -> s { funcs = insert (name, func) (funcs s) })

-- If else then processing for conditional statements
process (If condition thenBlock elseBlock) = do
    st <- liftState $ get
    case eval (vars st) (funcs st) condition of
        Right (BoolVal b) -> if b
                              then process (Block [thenBlock])
                              else process (Block [elseBlock])
        Left e -> liftIO $ putStrLn (show e)

-- While loop process
process (While cond block) = do
    st <- liftState $ get
    case eval (vars st) (funcs st) cond of
        Right (BoolVal b) -> if b then do process (Block [block]) -- execute iteration
                                          process (While cond block) -- recursive call enters next iteration
                             else return () -- end of loop
        Left e -> liftIO $ putStrLn (show e) -- error with conditional

-- For loop process
process (For cond op block) = do
    st <- liftState $ get
    case eval (vars st) (funcs st) cond of 
        Right (BoolVal b) -> if b then do process (Block [block]) -- execute iteration
                                          process op -- execute updater
                                          process (For cond op block) -- recursive call enters next iteration
                                else return () -- end of loop
        Left e -> liftIO $ putStrLn(show e) -- error with conditional

-- Print all available functionality/commands
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
    liftIO $ putStrLn ("  - if (x) then {y} else {z} {If then else conditionals")
    liftIO $ putStrLn ("List of program commands: ")
    liftIO $ putStrLn ("  - print ... {Print the Command}")
    liftIO $ putStrLn ("  - repeat n {} {Repeat an operation, n times")
    liftIO $ putStrLn ("  - while (x) {} {While x, execute operation}")
    liftIO $ putStrLn ("  - for (x; y) {} {While x is true, execute operation, after each iteration, execute y}")
    liftIO $ putStrLn ("  - quit {Quit the Program}")
    liftIO $ putStrLn ("  - :load fileName {Load a File}")
    liftIO $ putStrLn ("  - :help {Show Program Commands")

-- Quit the program
process (Quit) = do liftIO $ putStrLn ("Exiting program")
                    liftIO exitSuccess

{- Load File
 -
 - Function to process a file name and load the REPL loop if provided input
 - is a valid option. Prints the appropriate error should any arrive
-}
loadFile :: Name -> IO ()
loadFile file = do
    result <- tryJust (guard . isDoesNotExistError) $
              evalStateT (runInputTBehavior (useFile file) defaultSettings (runExceptT (repl False))) initState
    case result of
        Left err -> putStrLn "Error: File not found"
        Right (Left err) -> do putStrLn $ show err
        Right (Right newState) -> return ()

{- Read, Eval, Print Loop. This reads and parses the input using the pCommand parser.
 - calls 'process' to process the command. 'process' calls 'repl' when done, so the system loops.
 - Loop takes a boolean condition each run to check if the input being provided is user input or
 - file read. If file read, no additional output is printed.
 -}
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
            let parsed = parse pCommand (dropWhile isSpace (dropWhileEnd isSpace line)) -- trim surrounding whitespace
            case parsed of
                [(Right cmd, "")] -> process cmd
                [(Right cmd, rem)] -> liftIO $ putStrLn ("ParseError: Unconsumed input '" ++ rem ++ "'")
                [(Left err, _)] -> liftIO $ putStrLn (show err)
            repl continue

{- Run REPL is the initial call to run the REPL loop when the program is first run. Splits
 - the REPL loop segmentation into two processes to avoid IO/StateT collision errors.
 -}
runREPL :: Env -> IO ()
runREPL initState = do
    result <- evalStateT (runInputT hlSettings (runExceptT (repl True))) initState
    case result of
        Left err -> putStrLn $ show err
        Right _ -> return ()

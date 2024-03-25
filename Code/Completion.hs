module Completion where

import Data.List
import Control.Monad.IO.Class
import Control.Monad.State

import Expr
import BinaryTree

import System.Console.Haskeline

hlSettings :: MonadIO m => Settings (StateT Env m )
hlSettings = setComplete completions defaultSettings

-- Operations should act as separators
hl_separators = " (,*^/"

completions :: MonadIO m => CompletionFunc (StateT Env m)
completions = completeWordWithPrev Nothing hl_separators completor

completor :: MonadIO m => String -> String -> (StateT Env m) [Completion]
completor prev cur = if null prev then completeFirst cur
                     else completeVarOrFunc cur
                     --else if (reverse prev) == ":load" then listFiles cur


-- Commands that can easily be completed
firstWordCompletions :: [String]
firstWordCompletions = ["print", "repeat", ":quit", ":load", ":help", "for", "while", "if", "def"]

completeFirst :: Monad m => String -> m [Completion]
completeFirst s = return $ map simpleCompletion matching
    where matching = filter (isPrefixOf s) firstWordCompletions

builtInFuncs :: [String]
builtInFuncs = ["sqrt", "pow", "abs"]

completeVarOrFunc :: Monad m => String -> (StateT Env m) [Completion]
completeVarOrFunc s = do
    varCompls <- completeVar s
    fCompls <- completeFunc s
    return $ fCompls ++ varCompls

completeVar :: Monad m => String -> (StateT Env m) [Completion]
completeVar s = do
    state <- get
    let varMatches = completeFromTree s (vars state)
    return $ map simpleCompletion varMatches

completeFunc :: Monad m => String -> (StateT Env m) [Completion]
completeFunc s = do
    state <- get
    let builtInMatches = filter (isPrefixOf s) builtInFuncs
    let userMatches = completeFromTree s $ funcs state
    let allMatches = builtInMatches ++ userMatches
    let funcs = map (\s -> s ++ "(") allMatches
    return $ map (\s -> (simpleCompletion s) {isFinished=False}) funcs

{- Finds all tab-completions for the current input string given a binary tree
 - ordered by the strings.
 -
 - We use the the knowledge that the binary tree is ordered to reduce the
 - number of comparisons we have to make.
 -
 - Current input -> Tree -> possible completions
 -}
completeFromTree :: String -> Tree (String, a) -> [String]
completeFromTree s Empty = []
completeFromTree s (Tree (k, _) left right) = case compare k s of
                                                EQ -> k:(completeFromTree s right) -- longer completions lay here too
                                                LT -> completeFromTree s right
                                                {- If the current node is greater than,
                                                   it may be longer but starting with the correct string-}
                                                GT -> if isPrefixOf s k
                                                         then k:((completeFromTree s left) ++ (completeFromTree s right))
                                                         else completeFromTree s left

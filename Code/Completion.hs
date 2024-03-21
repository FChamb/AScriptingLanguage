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
hl_separators = " ,*^/"

completions :: MonadIO m => CompletionFunc (StateT Env m)
completions = completeWordWithPrev Nothing hl_separators completor

completor :: MonadIO m => String -> String -> (StateT Env m) [Completion]
completor prev cur = if null prev then completeFirst cur
                     else completeVarOrFunc cur


-- Commands that can easily be completed
firstWordCompletions :: [String]
firstWordCompletions = ["print", ":f", ":q"]

completeFirst :: Monad m => String -> m [Completion]
completeFirst s = return $ map simpleCompletion matching
    where matching = filter (isPrefixOf s) firstWordCompletions


completeVarOrFunc :: Monad m => String -> (StateT Env m) [Completion]
completeVarOrFunc s = do
        state <- get
        let funcCompletions = map (\s -> s ++ "(") $ completeFromTree s (funcs state)
        let varCompletions = completeFromTree s $ vars state
        return $ map simpleCompletion (funcCompletions ++ varCompletions)

-- Current input -> Tree -> possible completions
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

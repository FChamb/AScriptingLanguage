module Completion where

import Data.List

import Expr
import BinaryTree

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

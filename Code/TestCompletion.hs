{-# LANGUAGE TemplateHaskell #-}
module TestCompletion where

import Data.List

import BinaryTree
import Completion

import Test.QuickCheck

newtype IdentStr = IdentStr String
    deriving (Show)

instance Arbitrary IdentStr where
    arbitrary = do
        first <- letter
        rest <- resize 4 $ listOf letter
        return $ IdentStr (first:rest)
        where
            letter :: Gen Char
            letter = elements ['a','b','c']

prop_searchWorks :: IdentStr -> [(IdentStr, Int)] -> Property
prop_searchWorks (IdentStr search) list =
    collect (length expected) $ (sort (completeFromTree search tree) === expected)
    where
        l = map (\((IdentStr s), i) -> (s, i)) list
        l' = nubBy (\(s1, _) (s2, _) -> s1 == s2) l
        keyList = map fst l'
        expected = sort $ filter (isPrefixOf search) keyList
        tree = treeFromList l'

return []
runCompletionTests = $quickCheckAll

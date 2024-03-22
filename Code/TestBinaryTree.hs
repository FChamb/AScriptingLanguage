{-# LANGUAGE TemplateHaskell #-}
module TestBinaryTree where

import Data.List (nubBy)

import Test.QuickCheck

import BinaryTree

newtype BasicStr = BasicStr String
    deriving (Eq, Ord)

instance Arbitrary BasicStr where
    arbitrary = do
            s <- listOf (elements permitted)
            return $ BasicStr s
        where permitted = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

unwrapStr :: BasicStr -> String
unwrapStr (BasicStr s) = s

unwrapStrL :: [(BasicStr, a)] -> [(String, a)]
unwrapStrL = map (\(s, i) -> (unwrapStr s, i))

instance Show BasicStr where
    show (BasicStr s) = "\"" ++ s ++ "\""


prop_testInsertEmpty :: (BasicStr, Int) -> Property
prop_testInsertEmpty p = (insert p Empty === Tree p Empty Empty)

prop_testTreeContains :: [(BasicStr, Int)] -> Property
prop_testTreeContains l = conjoin $ map (expectInTree tree) ul
    where
        ul = unwrapStrL l
        tree :: Tree (String, Int)
        tree = uniqueKeyTree l
        expectInTree :: Tree (String, Int) -> (String, Int) -> Property
        expectInTree tree (k,_) = counterexample (show k) (contains k tree)

prop_testTreeValue :: [(BasicStr, Int)] -> Property
prop_testTreeValue l = conjoin $ map (expectValInTree tree) ul
    where
        ul = unwrapStrL l
        tree :: Tree (String, Int)
        tree = uniqueKeyTree l
        expectValInTree tree (k,v) = value k tree === Right v

uniqueKeyTree :: [(BasicStr, Int)] -> Tree (String, Int)
uniqueKeyTree l = treeFromList $ nubBy (\(s1, _) (s2, _) -> s1 == s2) (unwrapStrL l)

return []
runBinaryTreeTests = $quickCheckAll

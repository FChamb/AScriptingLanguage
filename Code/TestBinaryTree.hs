{-# LANGUAGE TemplateHaskell #-}
module TestBinaryTree where

import Data.List (nubBy)
import Data.Either (isLeft)

import Test.QuickCheck

import BinaryTree

newtype BasicStr = BasicStr String
    deriving (Eq, Ord)

instance Arbitrary BasicStr where
    arbitrary = do
            s <- listOf (elements permitted)
            return $ BasicStr s
        where permitted = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

newtype TreeNodes = TreeNodes [(String, Int)]
    deriving (Show)

instance Arbitrary TreeNodes where
    arbitrary = do
            l <- arbitrary
            let l' = nubBy (\(s1, _) (s2, _) -> s1 == s2) (unwrapStrL l)
            return $ TreeNodes l'

unwrapStr :: BasicStr -> String
unwrapStr (BasicStr s) = s

unwrapStrL :: [(BasicStr, a)] -> [(String, a)]
unwrapStrL = map (\(s, i) -> (unwrapStr s, i))

instance Show BasicStr where
    show (BasicStr s) = "\"" ++ s ++ "\""

prop_testInsertEmpty :: (BasicStr, Int) -> Property
prop_testInsertEmpty p = (insert p Empty === Tree p Empty Empty)

prop_testTreeValue :: TreeNodes -> Property
prop_testTreeValue (TreeNodes l) = conjoin $ map (expectValInTree tree) l
    where
        tree = treeFromList l
        expectValInTree tree (k,v) = value k tree === Right v

prop_testTreeNotValue :: TreeNodes -> BasicStr -> Property
prop_testTreeNotValue (TreeNodes l) (BasicStr k) =
    case (lookup k l) of
        Just _ -> discard
        Nothing -> property $ isLeft (value k tree)
    where
        tree = treeFromList l

return []
runBinaryTreeTests = $quickCheckAll

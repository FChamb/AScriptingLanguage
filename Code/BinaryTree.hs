module BinaryTree where

import Error

{- Structure of the Binary Search Tree. Either an empty tree or a tree with
 - two subtrees.
 -}
data Tree a = Empty | Tree a (Tree a) (Tree a)
    deriving (Show, Eq)

-- Function to calculate the height of the tree
height :: Tree a -> Int
height Empty = 0
height (Tree _ l r) = 1 + max (height l) (height r)

-- Function to calculate the balance factor of a sub tree in the main tree
balanceFactor :: Tree a -> Int
balanceFactor Empty = 0
balanceFactor (Tree _ l r) = height l - height r

-- Function to perform a right move on the tree
moveRight :: Tree a -> Tree a
moveRight (Tree x (Tree y ll lr) r) = Tree y ll (Tree x lr r)
moveRight t = t

-- Function to perform a left move on the main tree
moveLeft :: Tree a -> Tree a
moveLeft (Tree x l (Tree y rl rr)) = Tree y (Tree x l rl) rr
moveLeft t = t

-- Combines the previous functions to perform right then left move on the tree
moveRightThenLeft :: Tree a -> Tree a
moveRightThenLeft (Tree x l r) = moveLeft (Tree x (moveRight l) r)
moveRightThenLeft t = t

-- Combines moveRight and moveLeft to perform a left then right move on the tree
moveLeftThenRight :: Tree a -> Tree a
moveLeftThenRight (Tree x l r) = moveRight (Tree x l (moveLeft r))
moveLeftThenRight t = t

-- Function to balance the tree
balance :: Tree a -> Tree a
balance t
    | balanceFactor t > 1 = if balanceFactor (left t) >= 0 then moveRight t else moveLeftThenRight t
    | balanceFactor t < -1 = if balanceFactor (right t) <= 0 then moveLeft t else moveRightThenLeft t
    | otherwise = t
        where
          left (Tree _ l _) = l
          right (Tree _ _ r) = r

-- Function to insert a key value pair into the binary tree. Balances the tree on success
insert :: Ord n => (n, v) -> Tree (n, v) -> Tree (n, v)
insert (n, v) Empty = Tree (n, v) Empty Empty
insert (n, v) (Tree node l r) = balance $ case compare n (fst node) of
    EQ -> Tree (n, v) l r
    LT -> Tree node (insert (n, v) l) r
    GT -> Tree node l (insert (n, v) r)

-- Function to get the value associated with a key from the binary tree
value :: Ord n => n -> Tree (n, v) -> Either Error v
value _ Empty = Left $ TreeError "value not found"
value n (Tree node l r) = case compare n (fst node) of
    EQ -> Right (snd node)
    LT -> value n l
    GT -> value n r

-- Function to remove a key value pair from the tree (i.e subtree). Balance the tree on success
remove :: Ord n => n -> Tree (n, v) -> Tree (n, v)
remove _ Empty = Empty
remove n (Tree node l r) = balance $ case compare n (fst node) of
    EQ -> delete (Tree node l r)
    LT -> Tree node (remove n l) r
    GT -> Tree node l (remove n r)

-- Function to delete a sub tree from the binary search tree
delete :: Ord n => Tree (n, v) -> Tree (n, v)
delete (Tree _ l Empty) = l
delete (Tree _ Empty r) = r
delete (Tree _ l r)
  = Tree small l (remove (fst small) r)
  where small = smallestNode r

-- Function to find the smallest sub tree in the tree
smallestNode :: Tree (n, v) -> (n, v)
smallestNode (Tree node Empty _) = node
smallestNode (Tree _ l _) = smallestNode l

-- Function to create a binary tree from a list of key value pairs
treeFromList :: Ord n => [(n, v)] -> Tree (n, v)
treeFromList = foldl (\tree pair -> insert pair tree) Empty

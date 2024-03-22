module BinaryTree where

import Error

data Tree a = Empty | Tree a (Tree a) (Tree a)
    deriving (Show, Eq)

height :: Tree a -> Int
height Empty = 0
height (Tree _ l r) = 1 + max (height l) (height r)

balanceFactor :: Tree a -> Int
balanceFactor Empty = 0
balanceFactor (Tree _ l r) = height l - height r

moveRight :: Tree a -> Tree a
moveRight (Tree x (Tree y ll lr) r) = Tree y ll (Tree x lr r)
moveRight t = t

moveLeft :: Tree a -> Tree a
moveLeft (Tree x l (Tree y rl rr)) = Tree y (Tree x l rl) rr
moveLeft t = t

moveRightThenLeft :: Tree a -> Tree a
moveRightThenLeft (Tree x l r) = moveLeft (Tree x (moveRight l) r)
moveRightThenLeft t = t

moveLeftThenRight :: Tree a -> Tree a
moveLeftThenRight (Tree x l r) = moveRight (Tree x l (moveLeft r))
moveLeftThenRight t = t

balance :: Tree a -> Tree a
balance t
    | balanceFactor t > 1 = if balanceFactor (left t) >= 0 then moveRight t else moveLeftThenRight t
    | balanceFactor t < -1 = if balanceFactor (right t) <= 0 then moveLeft t else moveRightThenLeft t
    | otherwise = t
        where
          left (Tree _ l _) = l
          right (Tree _ _ r) = r

insert :: Ord n => (n, v) -> Tree (n, v) -> Tree (n, v)
insert (n, v) Empty = Tree (n, v) Empty Empty
insert (n, v) (Tree node l r) = balance $ case compare n (fst node) of
    EQ -> Tree (n, v) l r
    LT -> Tree node (insert (n, v) l) r
    GT -> Tree node l (insert (n, v) r)

value :: Ord n => n -> Tree (n, v) -> Either Error v
value _ Empty = Left $ TreeError "value not found"
value n (Tree node l r) = case compare n (fst node) of
    EQ -> Right (snd node)
    LT -> value n l
    GT -> value n r

remove :: Ord n => n -> Tree (n, v) -> Tree (n, v)
remove _ Empty = Empty
remove n (Tree node l r) = balance $ case compare n (fst node) of
    EQ -> delete (Tree node l r)
    LT -> Tree node (remove n l) r
    GT -> Tree node l (remove n r)

delete :: Ord n => Tree (n, v) -> Tree (n, v)
delete (Tree _ l Empty) = l
delete (Tree _ Empty r) = r
delete (Tree _ l r)
  = Tree small l (remove (fst small) r)
  where small = smallestNode r

smallestNode :: Tree (n, v) -> (n, v)
smallestNode (Tree node Empty _) = node
smallestNode (Tree _ l _) = smallestNode l

contains :: Ord n => n -> Tree (n, v) -> Bool
contains _ Empty = False
contains n (Tree node l r) = case compare n (fst node) of
    EQ -> True
    LT -> (contains n l)
    GT -> (contains n r)

treeFromList :: Ord n => [(n, v)] -> Tree (n, v)
treeFromList = foldl (\tree pair -> insert pair tree) Empty

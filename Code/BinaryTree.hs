module Struct.BinaryTree where

data Tree a = Empty | Tree a (Tree a) (Tree a)
    deriving (Show, Eq)

insert :: Ord n => (n, v) -> Tree (n, v) -> Tree (n, v)
insert (n, v) Empty = Tree (n, v) Empty Empty
insert (n, v) (Tree node l r) = case compare n (fst node) of
    EQ -> Tree (n, v) l r
    LT -> Tree node (insert (n, v) l) r
    GT -> Tree node l (insert (n, v) r)

value :: Ord n => n -> Tree (n, v) -> Either String v
value n Empty = Left "Value not found"
value n (Tree node l r) = case compare n (fst node) of
    EQ -> Right (snd node)
    LT -> value n l
    GT -> value n r

remove :: Ord n => n -> Tree (n, v) -> (Tree (n, v))
remove n Empty = undefined
remove n (Tree node l r) = case compare n (fst node) of
    EQ -> delete (Tree node l r)
    LT -> Tree node (remove n l) r
    GT -> Tree node l (remove n r)

delete :: Ord n => Tree (n, v) -> Tree (n, v)
delete (Tree node l Empty) = l
delete (Tree node Empty r) = r
delete (Tree small l r)
  = Tree small l (remove (fst small) r)
  where small = smallestNode r

smallestNode :: Tree (n, v) -> (n, v)
smallestNode (Tree node Empty _) = node
smallestNode (Tree _ l _) = smallestNode l

contains :: Ord n => n -> Tree (n, v) -> Bool
contains n Empty = False
contains n (Tree node l r) = case compare n (fst node) of
    EQ -> True
    LT -> (contains n l)
    GT -> (contains n r)

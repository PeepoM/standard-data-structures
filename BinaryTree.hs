module BinaryTree where

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

-- Checks if tree is empty
isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _ = False

-- Checks if tree is a binary search tree
isBST :: Ord a => Tree a -> Bool
isBST Leaf = True
isBST (Node left y right)
  | isEmpty left && isEmpty right = True
  | isEmpty left = isBST right && y < mint right
  | isEmpty right = isBST left && y > maxt left
  | otherwise = isBST left && isBST right && y > maxt left && y < mint right

-- Retrieves a min value in a given tree
mint :: Ord a => Tree a -> a
mint Leaf = error "The tree is empty!"
mint (Node left y right) = min y $ min leftSub rightSub
  where
    leftSub = if isEmpty left then y else mint left
    rightSub = if isEmpty right then y else mint right

-- Retrieves a max value in a given tree
maxt :: Ord a => Tree a -> a
maxt Leaf = error "The tree is empty!"
maxt (Node left y right) = max y $ max leftSub rightSub
  where
    leftSub = if isEmpty left then y else maxt left
    rightSub = if isEmpty right then y else maxt right

-- Checks if binary tree contains a given element
contains :: Ord a => a -> Tree a -> Bool
contains x Leaf = False
contains x (Node left y right)
  | x == y = True
  | otherwise = contains x left || contains x right

-- Checks if bianry SEARCH tree contains a given element
containsBST :: Ord a => a -> Tree a -> Bool
containsBST x Leaf = False
containsBST x (Node left y right)
  | x < y = containsBST x left
  | x > y = containsBST x right
  | otherwise = True

-- Returns the size of a given tree
size :: Tree a -> Int
size Leaf = 0
size (Node left y right) = 1 + size left + size right

-- Return the depth of a given tree
depth :: Tree a -> Int
depth Leaf = 0
depth (Node left y right) = 1 + max (depth left) (depth right)

-- Inserts an element to a tree
insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node Leaf x Leaf
insert x (Node left y right)
  | x < y = Node (insert x left) y right
  | x > y = Node left y (insert x right)
  | otherwise = Node left y right

-- Sample tree
test :: Tree Int
test = Node (Node (Node Leaf 3 Leaf) 4 (Node Leaf 5 Leaf)) 6 (Node Leaf 7 Leaf)
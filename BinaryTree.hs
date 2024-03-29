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
size (Node left _ right) = 1 + size left + size right

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

-- Function that uses depth-first search to find a node that satisfies a given predicate
dfs :: Eq a => (a -> Bool) -> Tree a -> Maybe a
dfs _ Leaf = Nothing
dfs p (Node left y right)
  | p y = Just y
  | leftSub == Nothing = rightSub
  | otherwise = leftSub
  where
    leftSub = dfs p left
    rightSub = dfs p right

-- Returns a list of visited nodes while traversing the tree in a depth-first manner
dfTraverse :: Tree a -> [a]
dfTraverse Leaf = []
dfTraverse (Node left y right) = y : leftSub ++ rightSub
  where
    leftSub = dfTraverse left
    rightSub = dfTraverse right

-- Converts a tree of "a" into a list of "a"
flatten :: Tree a -> [a]
flatten Leaf = []
flatten (Node left y right) = flatten left ++ [y] ++ flatten right

-- Function for folding lists to avoid repetitions
treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold base f Leaf = base
treeFold base f (Node left y right) = f (treeFold base f left) y (treeFold base f right)

-- Reiterating certain defined function once again, but this time in terms of treeFold
treeSize' :: Tree a -> Int
treeSize' = treeFold 0 (\left _ right -> 1 + left + right)

treeSum' :: Tree Int -> Int
treeSum' = treeFold 0 (\left y right -> left + y + right)

treeDepth' :: Tree a -> Int
treeDepth' = treeFold 0 (\left _ right -> 1 + max left right)

treeMax :: (Ord a, Bounded a) => Tree a -> a
treeMax = treeFold minBound (\left y right -> left `max` y `max` right)

-----------

-- Sample tree
test :: Tree Int
test = Node (Node (Node Leaf 3 Leaf) 4 (Node Leaf 5 Leaf)) 6 (Node Leaf 7 Leaf)
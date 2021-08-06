module LinkedList where

data List a = Nil | Cons a (List a) deriving (Show)

-- Returns the first element of a list
getFirst :: List a -> a
getFirst Nil = error "There are no elements in your list!"
getFirst (Cons x xs) = x

-- Returns the last element of a list
getLast :: List a -> a
getLast Nil = error "There are no elements in your list!"
getLast (Cons x Nil) = x
getLast (Cons x xs) = getLast xs

-- Adds an element to the beginning of a list
addFirst :: a -> List a -> List a
addFirst x xs = Cons x xs

-- Adds an element to the end of a list
addLast :: a -> List a -> List a
addLast x Nil = Cons x Nil
addLast x (Cons y ys) = Cons y (addLast x ys)

add :: a -> List a -> List a
add x xs = addLast x xs

-- Removes an element from a list
remove :: (Eq a) => a -> List a -> List a
remove _ Nil = error "There are no elements in your list!"
remove x (Cons y Nil)
  | x == y = Nil
  | otherwise = error "Your list does not contain the specified element!"
remove x (Cons y ys)
  | x == y = ys
  | otherwise = Cons y (remove x ys)

-- Returns an element at a specified index
get :: Int -> List a -> a
get _ Nil = error "There are no elements in your list!"
get index xs
  | index < 0 || index > (size xs) - 1 = error "Index out of bounds!"
  | otherwise = getHelper 0 index xs
  where
    getHelper :: Int -> Int -> List a -> a
    getHelper index targetIndex (Cons x xs)
      | index == targetIndex = x
      | otherwise = getHelper (index + 1) targetIndex xs

-- Returns an index of a specified element
indexOf :: (Eq a) => a -> List a -> Int
indexOf _ Nil = error "There are no elements in your list!"
indexOf x (Cons y ys) = indexOfHelper 0 x (Cons y ys)
  where
    indexOfHelper :: (Eq a) => Int -> a -> List a -> Int
    indexOfHelper index x (Cons y Nil)
      | x == y = index
      | otherwise = error "Your list does not contain the specified element!"
    indexOfHelper index x (Cons y ys)
      | x == y = index
      | otherwise = indexOfHelper (index + 1) x ys

-- Checks if an element is contained in a list
contains :: (Eq a) => a -> List a -> Bool
contains _ Nil = False
contains x (Cons y ys)
  | x == y = True
  | otherwise = contains x ys

-- Returns the size of a list
size :: List a -> Int
size Nil = 0
size (Cons x xs) = 1 + size xs

-- Sample list
test :: List Int
test = Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil))))
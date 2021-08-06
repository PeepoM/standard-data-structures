module Queue where

data Queue a = Queue [a] deriving (Eq, Show)

-- Adds an element to the beginning of a queue
add :: a -> Queue a -> Queue a
add x (Queue xs) = Queue (x : xs)

-- Removes an element from the beginning of a queue
remove :: Queue a -> (a, Queue a)
remove (Queue []) = error "The queue is empty!"
remove (Queue (x : xs)) = (x, Queue xs)

-- Returns the first element of a queue
peek :: Queue a -> a
peek (Queue []) = error "The queue is empty!"
peek (Queue (x : _)) = x

-- Sample queue
test :: Queue Int
test = Queue [1, 2, 3, 4]
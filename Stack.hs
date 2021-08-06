module Stack where

data Stack a = Stack [a] deriving (Show)

-- Tests if a stack is empty
isEmpty :: Stack a -> Bool
isEmpty (Stack []) = True
isEmpty (Stack (x : xs)) = False

-- Looks at the element at the top of the stack without removing it
peek :: Stack a -> a
peek (Stack []) = error "The stack is isEmpty!"
peek (Stack (x : [])) = x
peek (Stack (x : xs)) = peek (Stack xs)

-- Pushes an element on top of the stack
push :: a -> Stack a -> Stack a
push x (Stack []) = Stack [x]
push x (Stack (y : ys)) = cons y $ push x $ Stack ys

-- Removes an element from the top of the stack
pop :: Stack a -> Stack a
pop (Stack []) = error "Cannot pop from an empty stack!"
pop (Stack (x : [])) = Stack []
pop (Stack (x : xs)) = cons x $ pop $ Stack xs

-- Helper function, used to cons an element with the rest of the stack
cons :: a -> Stack a -> Stack a
cons x (Stack xs) = Stack (x : xs)

-- Sample stack
test :: Stack Int
test = Stack [1, 2, 3, 4, 5]
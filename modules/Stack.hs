module Stack where

data Stack a = Empty | Stk [a]
  deriving (Eq, Show)

push :: a -> Stack a -> Stack a
push x Empty = Stk [x]
push x (Stk xs) = Stk (x : xs)

pop :: Stack a -> Stack a
pop Empty = error "Stack.pop: empty stack"
pop (Stk (_ : xs)) = Stk xs

top :: Stack a -> a
top Empty = error "Stack.top: empty stack"
top (Stk (x : _)) = x

empty :: Stack a -> Stack a
empty _ = Stk []

isEmpty :: Stack a -> Bool
isEmpty Empty = False
isEmpty (Stk []) = True

module Queue where

data Queue a = Empty | Que [a]
  deriving (Eq, Show)

enqueue :: a -> Queue a -> Queue a
enqueue x Empty = Que [x]
enqueue x (Que xs) = Que (xs ++ [x])

dequeue :: Queue a -> Queue a
dequeue Empty = error "Queue.dequeue: empty queue"
dequeue (Que (x : xs)) = Que xs

front :: Queue a -> a
front Empty = error "Queue.front: empty queue"
front (Que (x : _)) = x

empty :: Queue a -> Queue a
empty _ = Que []

isEmpty :: Queue a -> Bool
isEmpty Empty = False
isEmpty (Que []) = True

module Set
  ( Set,
    isMember,
    insert,
    -- delete,
    -- union,
    -- intersection,
    empty,
    isEmpty,
    toList,
    fromList,
  )
where

-- Implementação com árvore binária (mais eficiente que listas)
data Set a
  = Empty -- Conjunto vazio
  | Node a (Set a) (Set a) -- Elemento, sub-conjuntos do menores e sub-conjunto dos maiores
  deriving (Eq, Show)

isMember :: (Ord a) => a -> Set a -> Bool
isMember x Empty = False
isMember x (Node y left right)
  | x == y = True
  | x > y = member x right
  | x < y = member x left

insert :: (Ord a) => a -> Set a -> Set a
insert x Empty = Node x Empty Empty
insert x (Node y left right)
  | x == y = Node y left right
  | x > y = Node y left (insert x right)
  | x < y = Node y (insert x left) right

-- delete :: (Ord a) => a -> Set a -> Set a
-- union :: a
-- intersection :: a

empty :: Set a
empty = Empty

isEmpty :: Set a -> Bool
isEmpty Empty = True
isEmpty _ = False

toList :: Set a -> [a]
toList Empty = []
toList (Node x l r) = toList l ++ [x] ++ toList r

fromList :: (Ord a) => [a] -> Set a
fromList = foldr insert Empty

module Map
  ( Map,
    insert,
    -- delete
  )
where

data Map key value = Empty | Node key value (Map key value) (Map key value)

insert :: (Ord k) => k -> v -> Map k v -> Map k v
insert x v Empty = Node x v Empty Empty
insert x v (Node y u l r)
  | x == y = Node x v l r
  | x > y = Node y u l (insert x v r)
  | x < y = Node y u (insert x v l) r

-- delete :: Ord k => k -> Map k v

lookup :: (Ord k) => k -> Map k v -> Maybe v
lookup x Empty = Nothing
lookup x (Node y v l r)
  | x == y = Just v
  | x > y = Map.lookup x r
  | x < y = Map.lookup x l

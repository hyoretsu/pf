-- Encaixe de padrões ("pattern matching" de linguagens funcionais, bastante poderoso quando se toca o que é)
not :: Bool -> Bool
not True = False
not False = True

(&&) :: Bool -> Bool -> Bool
-- True && True = True
-- True && False = False
-- False && True = False
-- False && False = False
False && _ = False
True && x = x
-- x && x = x // Não pode
-- _ && _ = x // Não pode
x && y | x == y = x
_ && _ = False

-- Padrões sobre tuplas (funciona em listas e virtualmente td)
first :: (a, b) -> a
first (x, _) = x

second :: (a, b) -> b
second (_, y) = y

-- Construir listas um a um acrescentando elementos a uma lista vazia com ":" (concatenação)
-- [1, 2, 3, 4] = 1 : (2 : (3 : (4 : [])))
[1, 2, 3, 4] = 1 : 2 : 3 : 4 : [] -- Forma melhor que ela não ensinou (usem array normal, funciona e é mais fácil)

small :: Int -> Bool
-- small x | x <= 2 = True
--         | otherwise = False
-- small x = if x <= 2 then True else False
small 0 = True
small 1 = True
small 2 = True
small _ = True

null :: [a] -> Bool
null xs = case xs of
  [] -> True
  -- (_:_) -> False
  _ -> False

tupledList = zip ['a', 'b', 'c'] [1, 2, 3, 4] -- Zip combina listas em tuplas
-- Equivalente: zip "abc" [1, 2, 3, 4]

indexOf :: (Eq a) => a -> [a] -> [Int]
indexOf x ys = [i | (y, i) <- zip ys [0 .. n], x == y] where n = length ys - 1 -- Junta em tuplas os valores da lista com seus índices (length ys - 1), checa a guarda que os valores são iguais a 'x' e retorna o índice dos resultados

consecutivePairs :: [a] -> [(a, a)]
consecutivePairs xs = zip xs (tail xs)

sameConsecutiveNumbersCount :: (Eq a) => [a] -> Int
sameConsecutiveNumbersCount xs = length [(x, x') | (x, x') <- zip xs (tail xs), x == x']

unzip :: [(x, y)] -> ([x], [y])
unzip zippedList = ([x | (x, y) <- zippedList], [y | (x, y) <- zippedList])

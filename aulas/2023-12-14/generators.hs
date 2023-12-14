-- List comprehension (usando geradores)
comprehendedList = [x ^ 2 | x <- [1, 2, 3, 4, 5]] -- x² para cada x dentro da lista [1, ..., 5]

generatedList = [(x, y) | x <- [1, 2, 3], y <- [4, 5]] -- Também serve para tuplas

genList2 = [(x, y) | x <- [1, 2, 3], y <- [x .. 3]] -- Geradores baseados em outros geradores (a ordem importa)

flattenedList = Prelude.concat [[1, 2, 3], [4, 5], [6, 7]] -- Parecido com o conceito de flattening

concat listas = [valor | lista <- listas, valor <- lista] -- Parecido com for dentro de for

guardedList = [x | x <- [1 .. 10], x `mod` 2 == 0] -- Filtros para os geradores

divisores :: Int -> [Int]
divisores n = [x | x <- [1 .. n], n `mod` x == 0]

isPrime :: Int -> Bool
isPrime n = divisores n == [1, n]

primeNumbers :: Int -> [Int]
primeNumbers n = [x | x <- [2 .. n], isPrime n]

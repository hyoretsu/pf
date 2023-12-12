-- Expressão lambda
-- Prompt> (\x -> 2 * x + 1) 1

-- Entender melhor currying
-- soma x y = x + y
soma = \x -> (\y -> x + y)

-- Equivalentes

-- Evitar dar nome a funções curtas
-- quadrados = map f [1..10]
--             where f x = x ^ 2
quadrados = map (\x -> x ^ 2) [1 .. 10]

-- Q1
removeAll :: (a -> Bool) -> [a] -> [a]
removeAll p xs = filter (\x -> not (p x)) xs -- Filtra o array dado pela negativa do predicado

-- removeAll even [1, 2, 3, 4] -- [1, 3]
-- removeAll (\x -> x == 2) [1, 2, 3, 4] -- [1, 3, 4]

-- Q2
printMean :: IO ()
printMean = do
  input <- getLine -- Pega o input
  let n = read input -- E converte para Int (assim como no slide)
  sum <- printMeanLoop n 0 -- Chama o while da soma
  print (fromIntegral sum / fromIntegral n) -- Printa a divisão dos dois números, convertendo-os para Fractional (necessário)

printMeanLoop :: Int -> Int -> IO Int
printMeanLoop 0 sum = return sum -- Retorna a soma se não precisa mais pegar números
printMeanLoop n sum = do
  input <- getLine -- Pega o input
  let number = read input -- E converte para Int (assim como no slide)
  printMeanLoop (n - 1) (sum + number) -- Continua o loop adicionando o número recebido à soma

-- 4
-- 10
-- 8
-- 3
-- 5
-- Saída: 6.5

-- Q3
data Command = Forward Int | Backward Int | TurnLeft | TurnRight
  deriving (Eq, Show)

destination :: [Command] -> (Int, Int)
destination steps = destinationStep steps (0, 0) Cima -- Chama o loop de passos inicializando no (0, 0) e para cima

data Direcao = Cima | Baixo | Esquerda | Direita -- Tipo utilitário para não usar 1, -1, 2, -2 como direções
  deriving (Eq) -- Precisa para usar '=='

destinationStep :: [Command] -> (Int, Int) -> Direcao -> (Int, Int)
-- Quando não há mais passos para dar, indepentente do eixo/direção, retorna a posição
destinationStep [] pos _ = pos
-- O comando dita o sinal e o eixo a posição do cálculo. Não tem muito mais que precise explicar porque fiz a solução mais simples que tinha. Após isso segue no loop até parar
destinationStep ((Forward n) : steps) (x, y) axis
  | axis == Cima = destinationStep steps (x, y + n) Cima
  | axis == Direita = destinationStep steps (x + n, y) Direita
  | axis == Baixo = destinationStep steps (x, y - n) Baixo
  | axis == Esquerda = destinationStep steps (x - n, y) Esquerda
destinationStep ((Backward n) : steps) (x, y) axis
  | axis == Cima = destinationStep steps (x, y - n) Cima
  | axis == Direita = destinationStep steps (x - n, y) Direita
  | axis == Baixo = destinationStep steps (x, y + n) Baixo
  | axis == Esquerda = destinationStep steps (x + n, y) Esquerda
-- Para comandos direcionais, simplesmente fica na mesma posição mas muda o eixo
destinationStep (TurnLeft : steps) (x, y) axis
  | axis == Cima = destinationStep steps (x, y) Esquerda
  | axis == Direita = destinationStep steps (x, y) Cima
  | axis == Baixo = destinationStep steps (x, y) Direita
  | axis == Esquerda = destinationStep steps (x, y) Baixo
destinationStep (TurnRight : steps) (x, y) axis
  | axis == Cima = destinationStep steps (x, y) Direita
  | axis == Direita = destinationStep steps (x, y) Baixo
  | axis == Baixo = destinationStep steps (x, y) Esquerda
  | axis == Esquerda = destinationStep steps (x, y) Cima

-- destination [Forward 2, TurnLeft, TurnLeft, Forward 1] -- (0,1)
-- destination [Backward 2, Forward 1] -- (0,-1)

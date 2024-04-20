--- QUESTÃO 1 ---
-- Definindo a função removeAll
removeAll :: (a -> Bool) -> [a] -> [a]

-- Caso base: se a lista for vazia, retorna uma lista vazia
removeAll _ [] = []

-- Caso recursivo: filtrar os elementos que não satisfazem o predicado p
removeAll p (x:xs)
  -- Se o predicado p é verdadeiro para o elemento x, não incluí-lo na lista resultante
  | p x       = removeAll p xs
  -- Se o predicado p é falso para o elemento x, incluí-lo na lista resultante
  | otherwise = x : removeAll p xs


--- QUESTÃO 2 ---
-- Função para ler os N números e calcular a média
printMean :: IO ()
printMean = do
    -- Lê a quantidade de números a serem lidos
    n <- readLn :: IO Int

    -- Lê os N números inteiros e armazena numa lista
    nums <- readInts n
  
    -- Calcula a média
    let mean = fromIntegral (sum nums) / fromIntegral n
  
    -- Imprime a média no terminal
    putStrLn $ show mean

-- Função para ler N números inteiros e armazená-los em uma lista
readInts :: Int -> IO [Int]
readInts 0 = return []  -- Caso base: lista vazia
readInts n = do
    -- Lê um número inteiro do terminal
    x <- readLn :: IO Int
    -- Chama recursivamente readInts para ler os próximos números
    xs <- readInts (n-1)
    -- Retorna a lista com o número lido acrescentado na frente
    return (x:xs)



-- QUESTÃO 3 --
-- definindo o tipo de Command
data Command = Forward Int | Backward Int | TurnLeft | TurnRight
  deriving (Eq, Show)

-- definindo a função destination
destination :: [Command] -> (Int, Int)
destination cmds = go cmds (0, 0) (0, 1) -- Começa na posição (0,0) e direção norte (0,1)
  where
    -- função aux para usar os comandos
    go :: [Command] -> (Int, Int) -> (Int, Int) -> (Int, Int)
    go [] pos _ = pos -- se não tivrr mais comandos retorna a posição atual
    go (cmd:rest) pos dir = case cmd of
      Forward n  -> go rest (moveForward pos dir n) dir -- avança n metros
      Backward n -> go rest (moveForward pos (invertDirection dir) n) dir -- volta n metros
      TurnLeft   -> go rest pos (turnLeft dir) -- gira p/ esquerda
      TurnRight  -> go rest pos (turnRight dir) -- gira p/ direita

    -- função aux para andar para frente
    moveForward :: (Int, Int) -> (Int, Int) -> Int -> (Int, Int)
    moveForward (x, y) (dx, dy) n = (x + n * dx, y + n * dy)

    -- função aux para inverter a direção
    invertDirection :: (Int, Int) -> (Int, Int)
    invertDirection (dx, dy) = (-dx, -dy)

    -- função aux para girar p/ esquerda
    turnLeft :: (Int, Int) -> (Int, Int)
    turnLeft (dx, dy) = (-dy, dx)

    -- função aux para girar p/ direita
    turnRight :: (Int, Int) -> (Int, Int)
    turnRight (dx, dy) = (dy, -dx)



-- programa main
main :: IO ()
main = do
    -- teste da função da questã 1 - removeAll
    putStrLn "\nQuestão 1: removeAll:"
    print $ removeAll even [1, 2, 3, 4]  -- tem que retornar: [1,3]
    print $ removeAll (\x -> x == 2) [1, 2, 3, 4]  -- tem que retornar: [1,3,4]

    -- teste da função da questão 2 - printMean
    putStrLn "\nQuestão 2: printMean:"
    printMean

    -- teste da função da questão 3 - destination
    putStrLn "\nQuestão 3: destination:"
    print $ destination [Forward 2, TurnLeft, TurnLeft, Forward 1]  -- Deve retornar (0,1)
    print $ destination [Backward 2, Forward 1]  -- Deve retornar (0,-1)
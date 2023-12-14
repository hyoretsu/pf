-- Criar uma sequência
list = [1 .. 10] -- 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
-- Sequência com salto

list2 = [1, 3 .. 9] -- 1, 3, 5, 7, 9
-- Sequência infinita (Em Haskell ela só é evaluada quando precisar ser usada)

infList = [1, 3 ..] -- "take 10" retira os 10 primeiros números da lista

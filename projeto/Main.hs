import Control.Monad (unless, when)
import Data.Char (toUpper)
import Data.List (transpose)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import Text.Printf (printf)

type Coord = (Int, Int)

type Line = [Int]

type Grid = [Line]

main :: IO ()
main = do
  start

start :: IO ()
start = do
  let board = [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]]
  board <- genNewTile board
  board <- genNewTile board
  gameLoop board 1

genNewTile :: Grid -> IO Grid
genNewTile board = do
  x <- randomRIO (0, 3)
  y <- randomRIO (0, 3)
  if board !! x !! y /= 0 then genNewTile board else updateBoard board (x, y) 2

updateBoard :: Grid -> Coord -> Int -> IO Grid
updateBoard board (i, j) num =
  let (upperRows, lowerRows) = splitAt i board
      (leftCells, rightCells) = splitAt j (head lowerRows)
   in return (upperRows ++ [leftCells ++ [num] ++ if null rightCells then [] else tail rightCells] ++ if null lowerRows then [] else tail lowerRows)

printBoard :: Grid -> Int -> IO Int
printBoard [] size = return size
printBoard (line : rest) size = do
  size <- printLine line size
  printBoard rest size

printLine :: Line -> Int -> IO Int
printLine nums size = do
  let maxNumber = maximum nums
  let newSize
        | maxNumber >= 10 && maxNumber < 100 = 2
        | maxNumber >= 100 && maxNumber < 1000 = 3
        | maxNumber >= 1000 = 4
        | otherwise = size

  putStrLn ("|" ++ concatMap (\num -> if num == 0 then replicate newSize '_' ++ "|" else printf ("%" ++ show newSize ++ "d") num ++ "|") nums)

  return newSize

gameLoop :: Grid -> Int -> IO ()
gameLoop board tileSize = do
  when (check2048 board) $ do
    putStrLn "Você venceu!"
    exitSuccess

  tileSize <- printBoard board tileSize

  putStrLn "Escolha um movimento (C, D, E, B): "
  line <- getLine
  let action = toUpper (head line)
  when (action /= 'C' && action /= 'D' && action /= 'E' && action /= 'B') $ error "Comando inválido"

  board <- move board action
  board <- genNewTile board

  gameLoop board tileSize

check2048 :: Grid -> Bool
check2048 board = any (elem 2048) board

move :: Grid -> Char -> IO Grid
move board 'C' = return (transpose (map lineShift (transpose board)))
move board 'D' = return $ map (reverse . lineShift . reverse) board
move board 'E' = return $ map lineShift board
move board 'B' = return (transpose (map (reverse . lineShift . reverse) (transpose board)))

-- Left
lineShift :: Line -> Line
lineShift line = merged ++ replicate (length line - length merged) 0
  where
    merged = mergeLine (filter (/= 0) line)
    mergeLine (x : y : xs)
      | x == y = (x * 2) : mergeLine xs
      | otherwise = x : mergeLine (y : xs)
    mergeLine x = x

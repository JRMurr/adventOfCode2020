module Main where

import Data.Maybe
import Data.List.Split

data Cell = Floor | Occupied | Empty deriving (Show, Eq)

data Grid = Grid {cells :: [[Cell]], width :: Int, height :: Int} deriving (Show)

type Cord = (Int, Int) -- (x,y)

tuplify2 :: [a] -> (a, a)
tuplify2 [x, y] = (x, y)

charToCell :: Char -> Cell
charToCell x = case x of
  '.' -> Floor
  'L' -> Empty
  '#' -> Occupied
  _ -> error "bad line"

parseLine :: String -> [Cell]
parseLine = map charToCell

getGrid :: [String] -> Grid
getGrid input =
  let cells = map parseLine input
   in Grid {cells = cells, width = length $ head cells, height = length cells}

getCell :: Grid -> Cord -> Maybe Cell
getCell grid (x, y)
  | width grid <= x || x < 0 = Nothing
  | height grid <= y || y < 0 = Nothing
  | otherwise = Just ((cells grid !! y) !! x)

-- zip [0..(width grid -1)] [0..(height grid - 1)]

runRuleOnCell :: Grid -> Cord -> Cell
runRuleOnCell grid (x0, y0) = case currentCell of
  Nothing -> error "cell does not exist"
  Just Floor -> Floor
  Just Occupied -> if numOccupied >= 4 then Empty else Occupied
  Just Empty -> if numOccupied == 0 then Occupied else Empty
  where
    adjacentCells = catMaybes [getCell grid (x, y) | x <- [x0 -1 .. x0 + 1], y <- [y0 -1 .. y0 + 1], not (x0 == x && y0 == y)]
    numOccupied = length (filter (== Occupied) adjacentCells)
    currentCell = getCell grid (x0, y0)

runRules:: Grid -> Grid
runRules grid =
  let allCords = [(x,y) | x <- [0..(width grid -1)], y <- [0..(height grid -1)]] in
    let newCells = chunksOf (width grid) (map ( runRuleOnCell grid) allCords) in
      if newCells == cells grid
        then grid
        else runRules grid {cells=newCells}

part1 :: IO ()
part1 = do
  lines <- getInput
  let grid = getGrid lines
  let finalGrid = runRules grid
  print finalGrid

part2 :: IO ()
part2 = do
  lines <- getInput
  print lines

getInput :: IO [String]
getInput = lines <$> readFile "./in.example"

main :: IO ()
main = part1
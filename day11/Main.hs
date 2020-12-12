module Main where
import Data.Maybe ( catMaybes, fromJust )
import Data.List ( intercalate )
import Data.Map (Map)
import qualified Data.Map as Map
data Cell = Floor | Occupied | Empty deriving (Show, Eq)
type Cord = (Int, Int) -- (x,y)
type Grid = Map Cord Cell

charToCell :: Char -> Cell
charToCell x = case x of
  '.' -> Floor
  'L' -> Empty
  '#' -> Occupied
  _ -> error "bad line"

parseLine :: String -> [Cell]
parseLine = map charToCell

mapWithIdx :: ((Int, a) -> b) -> [a] -> [b]
mapWithIdx f = zipWith (curry f) [0 .. ]


addCords :: [[Cell]] -> Grid
addCords lst = 
  Map.fromList (concat $ mapWithIdx (\ (y, row) -> mapWithIdx (\ (x, cell) -> ((x, y), cell)) row) lst)


getGrid :: [String] -> Grid
getGrid lst = addCords (map parseLine lst)

getCell :: Grid -> Cord -> Maybe Cell
getCell grid cord = Map.lookup cord grid

getAdjacentCells:: Grid -> Cord -> [Cell]
getAdjacentCells grid (x0, y0) = 
  catMaybes [getCell grid (x, y) | x <- [x0 -1 .. x0 + 1], y <- [y0 -1 .. y0 + 1], not (x0 == x && y0 == y)]

runRuleOnCellP1 :: Grid -> Cord -> Cell -> Cell
runRuleOnCellP1 grid currCord currentCell =
  case currentCell of
  Floor -> Floor
  Occupied -> if numOccupied >= 4 then Empty else Occupied
  Empty -> if numOccupied == 0 then Occupied else Empty
  where
    numOccupied = length (filter (== Occupied) (getAdjacentCells grid currCord))

runRules:: Grid -> (Grid -> Cord -> Cell -> Cell) -> Grid
runRules grid cellRule =
  let newGrid = Map.mapWithKey (cellRule grid) grid in
    if newGrid == grid 
      then grid
      else runRules newGrid cellRule

part1 :: IO ()
part1 = do
  lines <- getInput
  let grid = getGrid lines
  let finalGrid = runRules grid runRuleOnCellP1
  print $ length (Map.filter (==Occupied) finalGrid)

addSlope :: Cord -> (Int,Int) -> Cord
addSlope cord slope =
  (fst cord + fst slope, snd cord + snd slope)

followSlope:: Grid -> Cord -> (Int,Int) -> Maybe Cell
followSlope grid cord slope = 
  case getCell grid cord of
    Just Occupied -> Just Occupied
    Just Empty -> Just Empty
    Just Floor -> followSlope grid (addSlope cord slope) slope
    Nothing -> Nothing

getSeenCells :: Grid -> Cord -> [Cell]
getSeenCells grid currCord =
  catMaybes [followSlope grid (addSlope currCord (x,y)) (x, y) | x<-[-1..1], y<-[-1..1], not (x==0 && y==0)]


runRuleOnCellP2 :: Grid -> Cord -> Cell -> Cell
runRuleOnCellP2 grid currCord currentCell =
  case currentCell of
    Floor -> Floor
    Occupied -> if length occupiedCells >= 5 then Empty else Occupied
    Empty -> if null occupiedCells then Occupied else Empty
    where
      occupiedCells = filter (== Occupied) (getSeenCells grid currCord)

part2 :: IO ()
part2 = do
  lines <- getInput
  let grid = getGrid lines
  let finalGrid = runRules grid runRuleOnCellP2
  print $ length (Map.filter (==Occupied) finalGrid)

getInput :: IO [String]
getInput = lines <$> readFile "./in"


cellToChar :: Cell -> Char
cellToChar cell = 
  case cell of
    Empty -> 'L'
    Occupied -> '#'
    Floor -> '.'

showGrid :: Grid -> [Char]
showGrid m =
  let (x1,y1) = maximum $ Map.keys m in
  let (x0,y0) = minimum $ Map.keys m in
  let getRow y = [cellToChar $ fromJust (Map.lookup (x,y) m) | x<-[x0..x1]] in
  let lines = [getRow y | y<- [y0..y1]] in
  intercalate "\n" lines


main :: IO ()
main = part2
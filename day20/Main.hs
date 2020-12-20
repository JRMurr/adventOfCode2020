module Main where

import Data.Char
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import Data.List
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe

data Cell = On | Off deriving (Eq)

data Border = T | B | L | R

toCell :: Char -> Cell
toCell '#' = On
toCell '.' = Off

instance Show Cell where
  show On = "#"
  show Off = "."

type Tile = [[Cell]]

type TileMap = IntMap [Tile] -- all possible Orientation for tile

type Cord = (Int, Int)

type Grid = Map Cord (Int, Tile)

showTile :: Tile -> String
showTile t = intercalate "\n" (map (intercalate "" . map show) t)

parseId :: String -> Int
parseId s = read $ filter isDigit s

parseTile :: [String] -> (Int, Tile)
parseTile x =
  (id, map (map toCell) cells)
  where
    (idLine, cells) = fromJust $ uncons x
    id = read $ filter isDigit idLine

flipTileHor :: [a] -> [a]
flipTileHor = reverse

flipTileVert :: [[a]] -> [[a]]
flipTileVert = map reverse

rotateTile :: [[a]] -> [[a]]
rotateTile = transpose

allOrientations :: [[a]] -> [[[a]]]
allOrientations t =
  map
    (\f -> f t)
    [ id,
      flipTileHor,
      rotateTile,
      flipTileVert,
      flipTileVert . flipTileHor,
      flipTileHor . rotateTile,
      rotateTile . flipTileHor,
      flipTileVert . flipTileHor . rotateTile
    ]

getBorder :: Border -> Tile -> [Cell]
getBorder T t = head t
getBorder B t = last t
getBorder L t = map head t
getBorder R t = map last t

hasBorder :: Border -> [Cell] -> Tile -> Maybe Tile
hasBorder b c t =
  if getBorder b t == c then Just t else Nothing

getTilesWithBorder :: Border -> [Cell] -> [Tile] -> Maybe [Tile]
getTilesWithBorder b c tiles =
  if null filtered then Nothing else Just filtered
  where
    filtered = mapMaybe (hasBorder b c) tiles

-- need to remove already used tiles in map before calling
findTileWithBorder :: TileMap -> Border -> [Cell] -> [(Int, [Tile])]
findTileWithBorder tMap border cells =
  M.toList validTiles
  where
    validTiles = M.mapMaybe (getTilesWithBorder border cells) tMap

-- placeTile :: TileMap -> Int
-- placeTiles :: TileMap -> Grid
part1 :: IO ()
part1 = do
  input <- getInput
  -- print $ length input
  -- putStrLn $ intercalate "\n--------\n" (map show $ allOrientations [[1, 2, 3], [4, 5, 6], [7, 8, 9]])
  -- putStrLn $ intercalate "\n--------\n" (map show $ allOrientations [[1, 2], [3, 4]])
  -- let tMap = M.map allOrientations $ M.fromList input
  -- let ids =
  -- let firstTile = snd . head $ input
  -- let orientations = allOrientations firstTile
  -- putStrLn $ intercalate "\n--------\n" (map showTile orientations)
  -- putStrLn $ (showTile . snd) $ head input
  return ()

part2 :: IO ()
part2 = do
  input <- getInput
  return ()

main :: IO ()
main = part1

groupByBlank :: [String] -> [[String]]
groupByBlank = splitOn [""]

getInput :: IO [(Int, Tile)]
getInput = map parseTile . groupByBlank . lines <$> readFile "./in"
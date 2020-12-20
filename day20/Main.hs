module Main where

-- import Data.IntMap.Strict (IntMap)
-- import qualified Data.IntMap.Strict as M

import Data.Bifunctor (second)
import Data.Char
import Data.List
import Data.List.Split (chunksOf, splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Debug.Trace

type Mask = [String]

data Cell = On | Off deriving (Eq)

data Border = T | B | L | R

toCell :: Char -> Cell
toCell '#' = On
toCell '.' = Off

instance Show Cell where
  show On = "#"
  show Off = "."

type Tile = [[Cell]]

type Tiles = [(Int, [Tile])] -- all possible Orientation for each tile

type Cord = (Int, Int)

type Grid = Map Cord (Int, Tile)

addCords :: Cord -> Cord -> Cord
addCords (x, y) (x', y') =
  (x + x', y + y')

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

getTilesWithBorder :: Border -> [Cell] -> (Int, [Tile]) -> Maybe (Int, [Tile])
getTilesWithBorder b c (id, tiles) =
  if null filtered then Nothing else Just (id, filtered)
  where
    filtered = mapMaybe (hasBorder b c) tiles

-- need to remove already used tiles in map before calling
findTileWithBorder :: Tiles -> Border -> [Cell] -> [(Int, [Tile])]
findTileWithBorder tiles border cells =
  mapMaybe (getTilesWithBorder border cells) tiles

matchesAbove :: Grid -> Cord -> Tile -> Bool
matchesAbove grid cord tile =
  case neededCells of
    Just c -> currBorder == c
    Nothing -> False
  where
    aboveTile = Map.lookup (addCords cord (0, -1)) grid
    neededCells = fmap (getBorder B . snd) aboveTile
    currBorder = getBorder T tile

matchesLeft :: Grid -> Cord -> Tile -> Bool
matchesLeft grid cord tile =
  case neededCells of
    Just c -> currBorder == c
    Nothing -> False
  where
    leftTile = Map.lookup (addCords cord (-1, 0)) grid
    neededCells = fmap (getBorder R . snd) leftTile
    currBorder = getBorder L tile

getCord :: Int -> Int -> Cord
getCord len c =
  (c `mod` len, c `div` len)

deleteWithId :: Int -> Tiles -> Tiles
deleteWithId id = filter ((id /=) . fst)

placeHelper :: Int -> Int -> Grid -> Tiles -> [Grid]
placeHelper _ _ m [] = [m]
placeHelper len c m ts =
  [ m' | let cord = getCord len c, (id, tiles) <- ts, t <- tiles,
         -- valid if in first row or matches above
         snd cord == 0 || matchesAbove m cord t,
         -- valid if in first col or matches left
         fst cord == 0 || matchesLeft m cord t,
         -- valid so add to map and remove from possible tiles
         m' <- placeHelper len (c + 1) (Map.insert cord (id, t) m) $ deleteWithId id ts
  ]

placeTiles :: Tiles -> Grid
placeTiles allTiles = head $ placeHelper len 0 Map.empty allTiles
  where
    len = floor $ sqrt $ fromIntegral $ length allTiles -- grid width and height

-- placeTile :: TileMap -> Int
-- placeTiles :: TileMap -> Grid
part1 :: IO ()
part1 = do
  input <- getInput
  let tiles = map (second allOrientations) input
  let grid = placeTiles tiles
  let (r, c) = maximum $ Map.keys grid
  let corners = [(0, 0), (r, 0), (0, c), (r, c)]
  let cornerIds = map (fst . (grid Map.!)) corners
  print $ product cornerIds
  return ()

-- returns list of cords grouped by row
getAllCords :: Int -> [[Cord]]
getAllCords numTiles =
  groupBy (\(_, r) (_, r') -> r == r') [getCord len c | c <- [0 .. numTiles -1]]
  where
    len = floor $ sqrt $ fromIntegral numTiles

removeBorder :: Border -> Tile -> Tile
removeBorder L t = map (drop 1) t
removeBorder T t = drop 1 t
removeBorder B t = init t
removeBorder R t = map init t

mapWithIdx :: ((Int, a) -> b) -> [a] -> [b]
mapWithIdx f = zipWith (curry f) [0 ..]

getRow :: Int -> [Tile] -> [Cell]
getRow idx = concatMap (!! idx)

joinCols :: Grid -> [Cord] -> Tile
joinCols grid rowCords =
  mapWithIdx (\(idx, row) -> row ++ getRow idx (tail tiles)) (head tiles)
  where
    tiles = map (removeBorder B . removeBorder R . removeBorder L . removeBorder T . snd . (grid Map.!)) rowCords
    headTile = head tiles

joinGrid :: Grid -> Tile
joinGrid grid =
  concat rows
  where
    cords = getAllCords (Map.size grid)
    rows = map (joinCols grid) cords

checkCell :: (Char, Cell) -> Bool
checkCell ('#', On) = True
checkCell ('#', Off) = False
checkCell _ = True

checkRow :: (String, [Cell]) -> Maybe Int
checkRow (mask, row) =
  if isMatch then Just (length (filter ('#' ==) mask)) else Nothing
  where
    isMatch = all checkCell (zip mask row)

-- if mask matches returns number of '#' tiles that matched the mask
-- TODO: the mask is not the width of the tiles, need to add padding and check all paddings
checkChunk :: Mask -> Tile -> Maybe Int
checkChunk mask tile | length mask /= length tile = Nothing
checkChunk mask tile =
  if fullMatch then Just (sum (map fromJust checkedRows)) else Nothing
  where
    checkedRows = map checkRow (zip mask tile)
    fullMatch = all isJust checkedRows

checkTile :: Mask -> Tile -> Maybe Int
checkTile mask tile =
  if hasMatches then Just (sum (map (fromMaybe 0) checkedChunks)) else Nothing
  where
    chunks = chunksOf (length mask) tile
    checkedChunks = map (checkChunk mask) chunks
    hasMatches = trace (show checkedChunks) $ any isJust checkedChunks

-- if mask matches returns number of '#' tiles that matched the mask
getMatchedCount :: Mask -> Tile -> Int
getMatchedCount mask tile =
  fromJust $ fromJust (find isJust checkedOrientations)
  where
    orientations = allOrientations tile
    checkedOrientations = map (checkTile mask) orientations

part2 :: IO ()
part2 = do
  input <- getInput
  mask <- getMask
  let tiles = map (second allOrientations) input
  let grid = placeTiles tiles
  let image = joinGrid grid
  print $ getMatchedCount mask image
  return ()

main :: IO ()
main = part2

groupByBlank :: [String] -> [[String]]
groupByBlank = splitOn [""]

getMask :: IO [String]
getMask = lines <$> readFile "./mask"

getInput :: IO [(Int, Tile)]
getInput = map parseTile . groupByBlank . lines <$> readFile "./in.example"
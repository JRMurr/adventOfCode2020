module Main where
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe ( mapMaybe )
import qualified Data.Set as S

import Debug.Trace
data Dir = East | SouthEast | SouthWest | West | NorthWest | NorthEast deriving (Show, Eq, Ord)
type HexCord = (Int, Int, Int) -- (x,y,z) | z+y+z = 0 https://www.redblobgames.com/grids/hexagons/#coordinates-cube
data Color = White | Black deriving (Show, Eq)
type TileMap = Map HexCord Color

addCords :: HexCord -> HexCord -> HexCord
addCords (x,y,z) (x',y',z') = (x+x',y+y',z+z')

getCord:: [Dir] -> HexCord
getCord [] = (0,0,0)
getCord (East:t) = addCords (1,-1,0) $ getCord t
getCord (West:t) = addCords (-1,1,0) $ getCord t
getCord (NorthWest:t) = addCords (0,1,-1) $ getCord t
getCord (SouthEast:t) = addCords (0,-1,1) $ getCord t
getCord (NorthEast:t) = addCords (1,0,-1) $ getCord t
getCord (SouthWest:t) = addCords (-1,0,1) $ getCord t

flipTile :: TileMap -> HexCord -> TileMap
flipTile m c = 
  Map.alter alterCord c m
  where 
    alterCord (Just White) = Just Black  
    alterCord (Just Black) = Just White  
    alterCord Nothing = Just Black -- starts white so if not entry make it black


part1 :: IO ()
part1 = do
  input <- getInput
  let tiles = foldl flipTile Map.empty input
  print $ Map.size $ Map.filter (Black==) tiles
  return ()


directions3 :: [HexCord]
directions3 = [p | x <- [-1 .. 1], y <- [-1 .. 1], z <- [-1 .. 1], let p = (x, y, z), p /= (0, 0, 0), x+y+z == 0]

getColorOfCord :: HexCord -> TileMap  -> Color
getColorOfCord = Map.findWithDefault White

countBlack :: [Color] -> Int
countBlack = length . filter (== Black)

getBlackCount :: TileMap ->  HexCord-> Int
getBlackCount m c = countBlack . mapMaybe lookUpNeighbor $ directions3
  where
    lookUpNeighbor d = Map.lookup (addCords c d) m

rule:: TileMap -> HexCord -> TileMap -> TileMap
rule m c newMap = case (tileState, blackCount) of
  (Black, 0) -> Map.insert c White newMap
  (Black, n) | n > 2 -> Map.insert c White newMap
  (White, 2) -> Map.insert c Black newMap
  (x, _) -> Map.insert c x newMap
  where
    tileState = getColorOfCord c m
    blackCount = getBlackCount m c

nub' :: Ord a => [a] -> [a]
nub' = S.toList . S.fromList

-- get every neighbor from all cords in map since the tiles not in the map might be updated
area :: TileMap -> [HexCord]
area m = nub' $ [addCords p d | p <- Map.keys m, d <- directions3]

runStep :: TileMap -> TileMap
runStep m =
  foldl (\acc c -> rule m c acc) Map.empty (area m)

runSteps :: Int -> TileMap -> TileMap
runSteps 0 m = m
runSteps n m = runSteps (n -1) (runStep m)

getNumBlackTitles:: TileMap -> Int
getNumBlackTitles = Map.size . Map.filter (Black==)

part2 :: IO ()
part2 = do
  input <- getInput
  let initMap = foldl flipTile Map.empty input
  let tiles = runSteps 100 initMap
  print $ getNumBlackTitles tiles
  return ()

main :: IO ()
main = part2


parseLine:: String -> [Dir]
parseLine [] = []
parseLine ('s':'e':t) = SouthEast:(parseLine t)
parseLine ('s':'w':t) = SouthWest:(parseLine t)
parseLine ('n':'w':t) = NorthWest:(parseLine t)
parseLine ('n':'e':t) = NorthEast:(parseLine t)
parseLine ('e':t)  = East:(parseLine t)
parseLine ('w':t)  = West:(parseLine t)

getInput :: IO [HexCord]
getInput = map (getCord . parseLine) . lines <$> readFile "./in"
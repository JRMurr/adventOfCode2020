module Main where

import Data.Bifunctor (bimap, second)
import qualified Data.Map.Strict as M
import Data.Maybe (Maybe, fromJust, mapMaybe)
import qualified Data.Set as S

data State = Inactive | Active deriving (Eq, Show, Ord)

type Pos3 = (Int, Int, Int)

type Pos4 = (Int, Int, Int, Int)

type Grid d = M.Map d State

type Projection d = (d -> d -> d, [d])

d3 (x, y) = (x, y, 0)

d4 (x, y) = (x, y, 0, 0)

-- creates an XY map/chart
-- [".#.","#.."] => [
--                    ((0,0),'.'),((1,0),'#'),((2,0),'.'),
--                    ((0,1),'#'),((1,1),'.'),((2,1),'.')
--                  ]
indexXY :: Integral n => [[a]] -> [((n, n), a)]
indexXY xs = concat $ [[((x, y), c) | (x, c) <- zip [0 ..] r] | (y, r) <- zip [0 ..] xs]

parse :: Integral n => ((n, n) -> d) -> [String] -> [(d, State)]
parse fd = map (bimap fd state) . indexXY
  where
    state '.' = Inactive
    state '#' = Active

directions3 :: [Pos3]
directions3 = [p | x <- [-1 .. 1], y <- [-1 .. 1], z <- [-1 .. 1], let p = (x, y, z), p /= (0, 0, 0)]

directions4 :: [Pos4]
directions4 = [p | x <- [-1 .. 1], y <- [-1 .. 1], z <- [-1 .. 1], w <- [-1 .. 1], let p = (x, y, z, w), p /= (0, 0, 0, 0)]

proj3 :: Pos3 -> Pos3 -> Pos3
proj3 (x, y, z) (x', y', z') = (x + x', y + y', z + z')

proj4 :: Pos4 -> Pos4 -> Pos4
proj4 (x, y, z, w) (x', y', z', w') = (x + x', y + y', z + z', w + w')

grid :: (Integral n, Ord d) => ((n, n) -> d) -> [String] -> Grid d
grid dim = M.fromList . parse dim

countActive :: [State] -> Int
countActive = length . filter (== Active)

getActiveCount :: (Eq k, Ord k) => Grid k -> k -> Projection k -> Int
getActiveCount g point (proj, dir) = countActive . mapMaybe lookUpNeighbor $ dir
  where
    lookUpNeighbor d = M.lookup (proj point d) g

getStateOfPoint :: (Ord k) => Grid k -> k -> State
getStateOfPoint g p =
  M.findWithDefault Inactive p g

rule :: (Ord k) => Grid k -> Grid k -> k -> Projection k -> Grid k
rule g newGrid p proj = case (posState, activeCount) of
  (Inactive, 3) -> M.insert p Active newGrid
  (Active, n) | n == 2 || n == 3 -> M.insert p Active newGrid
  _ -> M.insert p Inactive newGrid
  where
    posState = getStateOfPoint g p
    activeCount = getActiveCount g p proj

nub' :: Ord a => [a] -> [a]
nub' = S.toList . S.fromList

area :: (Eq k, Ord k) => Grid k -> Projection k -> [k]
area g (proj, dir) = nub' $ [proj p d | p <- M.keys g, d <- dir]

runStep :: (Eq k, Ord k) => Projection k -> Grid k -> Grid k
runStep proj g =
  foldl (\acc p -> rule g acc p proj) M.empty (area g proj)

runSteps :: (Eq k, Ord k) => Int -> Projection k -> Grid k -> Grid k
runSteps 0 _ g = g
runSteps n p g = runSteps (n -1) p (runStep p g)

part1 :: IO ()
part1 = do
  input <- getInput
  let initGrid = grid d3 input
  let finalGrid = runSteps 6 (proj3, directions3) initGrid
  print $ countActive . M.elems $ finalGrid

part2 :: IO ()
part2 = do
  input <- getInput
  let initGrid = grid d4 input
  let finalGrid = runSteps 6 (proj4, directions4) initGrid
  print $ countActive . M.elems $ finalGrid

main :: IO ()
main = part2

getInput :: IO [String]
getInput = lines <$> readFile "./in"

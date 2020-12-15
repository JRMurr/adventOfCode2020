module Main where

import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace (trace)

getLast2Elems :: [a] -> [a]
getLast2Elems lst = drop (length lst - 2) lst

playGameSlow :: [Int] -> [Int]
playGameSlow nums =
  case indices of
    [_] -> nums ++ [0]
    _ ->
      let [x, y] = getLast2Elems indices
       in nums ++ [y - x]
  where
    lastNum = last nums
    indices = elemIndices lastNum nums

part1 :: IO ()
part1 = do
  input <- getInput
  let numRoundsToPlay = 2020 - (length input) - 1
  print $ last $ foldl (\acc _ -> playGameSlow acc) input ([0 .. numRoundsToPlay])

type LastSeen = Map Int Int

-- lastNum should not be in the map yet when it gets here
-- will be added during this func
-- returns the next number and the updated map
playGameFast :: LastSeen -> Int -> Int -> (Int, LastSeen)
playGameFast map turn lastNum =
  let nextNum =
        ( case Map.lookup lastNum map of
            Just idx -> (turn - 1) - idx
            Nothing -> 0
        )
   in (nextNum, Map.insert lastNum (turn -1) map)

getLastIndex :: [Int] -> Int -> Int
getLastIndex nums num =
  let indices = elemIndices num nums
   in last indices + 1

initGame :: [Int] -> (Int, Int, LastSeen)
initGame input =
  let map = foldl (\acc num -> Map.insert num (getLastIndex (reverse tail) num) acc) Map.empty tail
   in (nextTurnNum, lastNum, map)
  where
    nextTurnNum = length input + 1
    (lastNum, tail) = fromJust $ uncons (reverse input)

part2 :: IO ()
part2 = do
  input <- getInput
  let (firstTurn, lastNum, map) = initGame input
  print $ fst $ foldl (\(num, map) turn -> playGameFast map turn num) (lastNum, map) [firstTurn .. 30000000]

getInput :: IO [Int]
getInput = map read . splitOn "," <$> readFile "./in"

main :: IO ()
main = part2

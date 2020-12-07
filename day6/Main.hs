module Main where

import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as Set

groupByBlank :: [String] -> [[String]]
groupByBlank = splitOn [""]

toSets :: [String] -> [Set Char]
toSets lst =
  [Set.fromList (filter (/= ' ') $ unwords x) | x <- groupByBlank lst]

getGroupIntersection :: [String] -> [Set Char]
getGroupIntersection lst =
  map (foldl (\acc x -> acc `Set.intersection` Set.fromList x) (Set.fromList ['a' .. 'z'])) $
    groupByBlank lst

getInput :: IO [String]
getInput = do
  fp <- readFile "./in"
  return (lines fp)

part1 :: IO ()
part1 = do
  lines <- getInput
  let grouped = toSets lines
  print $ sum $ map length grouped

part2 :: IO ()
part2 = do
  lines <- getInput
  let grouped = getGroupIntersection lines
  print $ sum $ map length grouped

main :: IO ()
main = part2

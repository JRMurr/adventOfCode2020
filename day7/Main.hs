module Main where

import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map

getInput :: IO [String]
getInput = lines <$> readFile "./in"

trim = unwords . words

-- bag with capacity
type BagCount = (String, Int)

-- Tuple of outer bag with list of (bag, count)
type BagRule = (String, [BagCount])

parseSingleBagCount :: String -> BagCount
parseSingleBagCount str =
  case words $ trim str of
    [count, adjective, color, "bags"] -> (adjective ++ " " ++ color, read count)
    ["1", adjective, color, "bag"] -> (adjective ++ " " ++ color, 1)
    _ -> error "bad bag"

parseCounts :: String -> [BagCount]
parseCounts str =
  map parseSingleBagCount $ splitOn "," str

parseLine :: String -> BagRule
parseLine line =
  case splitOn "bags contain" line of
    [outer, " no other bags."] -> (trim outer, [])
    -- init to remove period at end of the inner str
    [outer, inner] -> (trim outer, parseCounts $ trim $ init inner)
    _ -> error "bad line"

type BagMap = Map String [BagCount]

getInnerBags :: [BagCount] -> [String]
getInnerBags =
  map fst

deepenBag :: BagMap -> String -> [String]
deepenBag bMap key =
  case Map.lookup key bMap of
    Just bagCounts ->
      let innerBags = getInnerBags bagCounts
       in innerBags `union` concatMap (deepenBag bMap) innerBags
    Nothing -> []

getTotalBagsInBag :: BagMap -> String -> Int
getTotalBagsInBag bMap key =
  case Map.lookup key bMap of
    Nothing -> 0
    Just bagCounts ->
      foldl (\acc (bag, count) -> acc + count * (getTotalBagsInBag bMap bag + 1)) 0 bagCounts

part1 = do
  input <- getInput
  let bMap = Map.fromList (map parseLine input)
  let filteredMap = Map.filterWithKey (\k _ -> "shiny gold" `elem` deepenBag bMap k) bMap
  print $ length filteredMap

part2 = do
  input <- getInput
  let bMap = Map.fromList (map parseLine input)
  print $ getTotalBagsInBag bMap "shiny gold"

main :: IO ()
main = part2

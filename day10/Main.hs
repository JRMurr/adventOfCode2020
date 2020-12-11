module Main where

import Data.Function
import Data.List
import Debug.Trace

joltDiff :: Int
joltDiff = 3

getValidAdapters :: [Int] -> Int -> [Int]
getValidAdapters adapters currentJolt =
  let valid = [(currentJolt + 1) .. (currentJolt + joltDiff)] `intersect` adapters
   in if null valid then error "no adapters" else valid

findFullChain :: [Int] -> Int -> [Int] -> [Int]
findFullChain adapters currentJolt usedAdapters
  | currentJolt + 1 > maximum adapters = (currentJolt + joltDiff) : usedAdapters
  | otherwise =
    let nextAdapter = minimum $ getValidAdapters adapters currentJolt
     in findFullChain adapters nextAdapter (nextAdapter : usedAdapters)

getDifferences :: [Int] -> [Int]
getDifferences = zipWith (-) <*> tail

part1 :: IO ()
part1 = do
  lines <- getInput
  let adapterChain = findFullChain lines 0 [0]
  let differences = getDifferences adapterChain
  let (threes, ones) = partition (== 3) differences
  print $ length threes * length ones

type Chain = [Int]

memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)

findAllAllowedChains :: [Int] -> Int
findAllAllowedChains adapters =
  let tmp f currentJolt
        | currentJolt + 1 > maximum adapters = 1
        | otherwise =
          sum [f jolt | jolt <- getValidAdapters adapters currentJolt]
   in fix (memoize . tmp) 0

part2 :: IO ()
part2 = do
  lines <- getInput
  print $ findAllAllowedChains lines

getInput :: IO [Int]
getInput = map read . lines <$> readFile "./in"

main :: IO ()
main = part2

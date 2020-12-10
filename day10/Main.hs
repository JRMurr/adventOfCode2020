module Main where

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

getDifferences = zipWith (-) <*> tail

part1 :: IO ()
part1 = do
  lines <- getInput
  let adapterChain = findFullChain lines 0 [0]
  let differences = getDifferences adapterChain
  let (threes, ones) = partition (== 3) differences
  print $ length threes * length ones

type Chain = [Int]

slice :: Int -> Int -> [a] -> [a]
slice start end = take (end - start + 1) . drop start

getValidAdaptersSorted :: [Int] -> Int -> [(Int, Int)]
getValidAdaptersSorted adapters idx =
  let currentJolt = adapters !! idx
   in let possibleWithIdx = zip [idx + 1 ..] (slice (idx + 1) (idx + joltDiff) adapters)
       in let valid = filter (\(_, jolt) -> jolt >= currentJolt + 1 && jolt <= currentJolt + joltDiff) possibleWithIdx
           in if null valid then error "no adapters" else valid

findAllAllowedChains :: [Int] -> Int -> [Chain]
findAllAllowedChains adapters idx
  | currentJolt + 1 > maximum adapters = [[currentJolt + joltDiff]]
  | otherwise =
    let allChainsAfterCurrent = [map (jolt :) (findAllAllowedChains adapters newIdx) | (newIdx, jolt) <- getValidAdaptersSorted adapters idx]
     in concat allChainsAfterCurrent
  where
    currentJolt = adapters !! idx

part2 :: IO ()
part2 = do
  lines <- getInput
  print $ length (findAllAllowedChains (0 : sort lines) 0)

getInput :: IO [Int]
getInput = map read . lines <$> readFile "./in"

main :: IO ()
main = part2

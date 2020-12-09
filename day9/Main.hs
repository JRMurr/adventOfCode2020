module Main where

import Data.List

slice :: Int -> Int -> [a] -> [a]
slice start end = take (end - start + 1) . drop start

toPairs :: Ord a => [a] -> [(a, a)]
toPairs l = [(x, y) | x <- l, y <- l, x < y]

checkIndex :: [Int] -> Int -> Int -> Bool
checkIndex lst currIndex preambleLen =
  let numbers = slice (currIndex - 1 - preambleLen) (currIndex - 1) lst
   in (lst !! currIndex) `elem` [x + y | (x, y) <- toPairs numbers]

findBadNum :: [Int] -> Int -> (Int, Int)
findBadNum lst preambleLen =
  let indexes = [preambleLen + 1 .. (length lst)]
   in let resIdx = find (\x -> not $ checkIndex lst x preambleLen) indexes
       in case resIdx of
            Just idx -> (lst !! idx, idx)
            Nothing -> error "not found"

findContiguousHelper :: [Int] -> Int -> Int -> Int -> Maybe [Int]
findContiguousHelper lst targetSum startIdx endIdx
  | endIdx == 0 = Nothing
  | startIdx == endIdx = findContiguousHelper lst targetSum 0 (endIdx - 1)
  | otherwise =
    let currSlice = slice startIdx endIdx lst
     in if sum currSlice == targetSum
          then Just currSlice
          else findContiguousHelper lst targetSum (startIdx + 1) endIdx

findContiguous :: [Int] -> Int -> [Int]
findContiguous lst targetSum =
  case findContiguousHelper lst targetSum 0 (length lst) of
    Just res -> res
    Nothing -> error "not found!!"

part1 :: IO ()
part1 = do
  lines <- getInput
  let input = map (read :: String -> Int) lines
  let preambleLen = 25
  print $ findBadNum input preambleLen

part2 :: IO ()
part2 = do
  lines <- getInput
  let input = map (read :: String -> Int) lines
  let preambleLen = 25
  let (bad_num, idx) = findBadNum input preambleLen
  let prevNums = slice 0 (idx -1) input
  let nums = findContiguous prevNums bad_num
  print nums
  print $ minimum nums + maximum nums

getInput :: IO [String]
getInput = lines <$> readFile "./in"

main :: IO ()
main = part2
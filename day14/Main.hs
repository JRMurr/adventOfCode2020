module Main where

import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map

type BinaryNum = [Int]

type MemAssignment = (Int, BinaryNum) -- (addr, value)

type MaskAssignments = (String, [MemAssignment])

type Memory = Map Int Int

-- writeValues :: Memory -> MaskAssignments -> Memory
toBinaryHelper :: Int -> BinaryNum
toBinaryHelper 0 = [0]
toBinaryHelper n =
  let (quot, rem) = n `quotRem` 2 in toBinaryHelper quot ++ [rem]

-- pads num with leading 0s
toBinary :: Int -> BinaryNum
toBinary n =
  let num = toBinaryHelper n
   in if length num >= 36
        then num
        else replicate (36 - length num) 0 ++ num

fromBinary :: BinaryNum -> Int
fromBinary lst =
  let lstWithIdx = zip [0 ..] (reverse lst)
   in sum [2 ^ idx | (idx, x) <- lstWithIdx, x == 1]

getMaskedDigit :: Char -> Int -> Int
getMaskedDigit '1' _ = 1
getMaskedDigit '0' _ = 0
getMaskedDigit _ digit = digit

getMaskedNum :: String -> BinaryNum -> BinaryNum
getMaskedNum mask num =
  zipWith
    (curry (\(char, digit) -> getMaskedDigit char digit))
    mask
    num

writeValue :: Memory -> String -> MemAssignment -> Memory
writeValue memory mask (addr, value) =
  let maskedVal = fromBinary (getMaskedNum mask value)
   in Map.insert addr maskedVal memory

writeMaskAssignments :: Memory -> MaskAssignments -> Memory
writeMaskAssignments memory (mask, assignments) =
  foldl (`writeValue` mask) memory assignments

writeAllMaskAssignments :: Memory -> [MaskAssignments] -> Memory
writeAllMaskAssignments = foldl writeMaskAssignments

part1 :: IO ()
part1 = do
  input <- getInput
  let memory = writeAllMaskAssignments Map.empty input
  print memory
  print $ sum (Map.elems memory)

part2 :: IO ()
part2 = do
  input <- getInput
  print input

replace old new = intercalate new . splitOn old

parseMemoryAssignment :: String -> MemAssignment
parseMemoryAssignment str =
  let [keyStr, value] = splitOn " = " str
   in let key = replace "]" "" (replace "mem[" "" keyStr)
       in (read key, toBinary $ read value)

parseMask :: String -> String
parseMask = replace "mask = " ""

getInput :: IO [MaskAssignments]
getInput = do
  fileContents <- readFile "./in"
  let chunked = map lines $ split (startsWith "mask = ") fileContents
  return (map (\x -> (parseMask $ head x, map parseMemoryAssignment (tail x))) chunked)

main :: IO ()
main = part1

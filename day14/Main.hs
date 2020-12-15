module Main where

import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map

type BinaryNum = [Int]

type MemAssignment = (Int, Int) -- (addr, value)

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

getMaskedDigitP1 :: Char -> Int -> Int
getMaskedDigitP1 '1' _ = 1
getMaskedDigitP1 '0' _ = 0
getMaskedDigitP1 _ digit = digit

getMaskedNumP1 :: String -> BinaryNum -> BinaryNum
getMaskedNumP1 mask num =
  zipWith
    (curry (\(char, digit) -> getMaskedDigitP1 char digit))
    mask
    num

writeValueP1 :: Memory -> String -> MemAssignment -> Memory
writeValueP1 memory mask (addr, value) =
  let maskedVal = fromBinary (getMaskedNumP1 mask (toBinary value))
   in Map.insert addr maskedVal memory

writeMaskAssignmentsP1 :: Memory -> MaskAssignments -> Memory
writeMaskAssignmentsP1 memory (mask, assignments) =
  foldl (`writeValueP1` mask) memory assignments

writeAllMaskAssignmentsP1 :: Memory -> [MaskAssignments] -> Memory
writeAllMaskAssignmentsP1 = foldl writeMaskAssignmentsP1

addToFrontOfAll :: Int -> [BinaryNum] -> [BinaryNum]
addToFrontOfAll newHead = map (newHead :)

getMaskedAddrs :: String -> BinaryNum -> [BinaryNum]
getMaskedAddrs mask binNum = case uncons mask of
  Nothing -> [binNum]
  Just ('0', t) -> addToFrontOfAll (head binNum) (getMaskedAddrs t (tail binNum))
  Just ('1', t) -> addToFrontOfAll 1 (getMaskedAddrs t (tail binNum))
  Just (_, t) ->
    let res = getMaskedAddrs t (tail binNum)
     in addToFrontOfAll 1 res ++ addToFrontOfAll 0 res

writeValueP2 :: Memory -> String -> MemAssignment -> Memory
writeValueP2 memory mask (addr, value) =
  let maskedAddrs = getMaskedAddrs mask (toBinary addr)
   in foldl (\acc v -> Map.insert (fromBinary v) value acc) memory maskedAddrs

writeMaskAssignmentsP2 :: Memory -> MaskAssignments -> Memory
writeMaskAssignmentsP2 memory (mask, assignments) =
  foldl (`writeValueP2` mask) memory assignments

writeAllMaskAssignmentsP2 :: Memory -> [MaskAssignments] -> Memory
writeAllMaskAssignmentsP2 = foldl writeMaskAssignmentsP2

part1 :: IO ()
part1 = do
  input <- getInput
  let memory = writeAllMaskAssignmentsP1 Map.empty input
  print $ sum (Map.elems memory)

part2 :: IO ()
part2 = do
  input <- getInput
  let memory = writeAllMaskAssignmentsP2 Map.empty input
  print $ sum (Map.elems memory)

replace old new = intercalate new . splitOn old

parseMemoryAssignment :: String -> MemAssignment
parseMemoryAssignment str =
  let [keyStr, value] = splitOn " = " str
   in let key = replace "]" "" (replace "mem[" "" keyStr)
       in (read key, read value)

parseMask :: String -> String
parseMask = replace "mask = " ""

getInput :: IO [MaskAssignments]
getInput = do
  fileContents <- readFile "./in"
  let chunked = map lines $ split (startsWith "mask = ") fileContents
  return (map (\x -> (parseMask $ head x, map parseMemoryAssignment (tail x))) chunked)

main :: IO ()
main = part2

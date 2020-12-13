module Main where
import Data.List
import Data.List.Split
parseInput :: [String] -> (Int, [(Int, Int)])
parseInput lines = case lines of
  [time, busIds] ->
    let busIdsList = zip [0..] (splitOn "," busIds)
     in (read time, [(idx, read x) | (idx,x) <- busIdsList, x /= "x"])
  _ -> error "bad input"

-- gets the decimal portion of the number
getDecimal :: Double -> Double
getDecimal = snd . properFraction

getCloseNess :: Int -> Int -> Double
getCloseNess arriveTime busId = getDecimal (fromIntegral arriveTime / fromIntegral busId)

getModRemiander num demon = 
  let modRes = num `mod` demon in
    demon - modRes

findNextBus :: Int -> [Int] -> (Int, Int)
findNextBus arriveTime busIds = 
  let nextBus = maximumBy (\x y -> getCloseNess arriveTime x `compare` getCloseNess arriveTime y) busIds in
  let comesIn = getModRemiander  arriveTime nextBus in
    (nextBus, comesIn)

-- isTimeValid :: Int -> [(Int, Int)] -> Bool
-- isTimeValid time = all (\(idx, busId) -> (time + idx) `mod` busId == 0)

isTimeValid :: Int -> [Int] -> Bool
isTimeValid time = all (\busId -> time `mod` busId == 0)


part1 :: IO ()
part1 = do
  lines <- getInput
  let (arriveTime, busIds) = parseInput lines
  let (nextBus, comesIn) = findNextBus arriveTime (map snd busIds)
  print $ (nextBus, comesIn)
  print $ nextBus * comesIn


part2 :: IO ()
part2 = do
  lines <- getInput
  let (_, busIds) = parseInput lines
  let shiftedTimes = map (\(idx, x) -> x - idx) busIds
  let biggest = maximum shiftedTimes
  let firstValidTime = find (\x -> isTimeValid x shiftedTimes) [biggest,biggest..] 
  print firstValidTime

getInput :: IO [String]
getInput = lines <$> readFile "./in"

main :: IO ()
main = part2
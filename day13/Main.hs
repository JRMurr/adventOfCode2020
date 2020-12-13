module Main where
import Data.List
import Data.List.Split
parseInput :: [String] -> (Int, [Int])
parseInput lines = case lines of
  [time, busIds] ->
    let busIdsList = splitOn "," busIds
     in (read time, [read x | x <- busIdsList, x /= "x"])
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

part1 :: IO ()
part1 = do
  lines <- getInput
  let (arriveTime, busIds) = parseInput lines
  let (nextBus, comesIn) = findNextBus arriveTime busIds
  print $ (nextBus, comesIn)
  print $ nextBus * comesIn

part2 :: IO ()
part2 = do
  lines <- getInput
  print lines

getInput :: IO [String]
getInput = lines <$> readFile "./in"

main :: IO ()
main = part1
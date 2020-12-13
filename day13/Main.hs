module Main where

import Data.List
import Data.List.Split

parseInput :: [String] -> (Int, [(Int, Int)])
parseInput lines = case lines of
  [time, busIds] ->
    let busIdsList = zip [0 ..] (splitOn "," busIds)
     in (read time, [(idx, read x) | (idx, x) <- busIdsList, x /= "x"])
  _ -> error "bad input"

-- gets the decimal portion of the number
getDecimal :: Double -> Double
getDecimal = snd . properFraction

getCloseNess :: Int -> Int -> Double
getCloseNess arriveTime busId = getDecimal (fromIntegral arriveTime / fromIntegral busId)

intDiv :: Int -> Int -> Int
intDiv x y = x `div` y

getModRemiander num demon =
  let modRes = num `mod` demon
   in demon - modRes

findNextBus :: Int -> [Int] -> (Int, Int)
findNextBus arriveTime busIds =
  let nextBus = maximumBy (\x y -> getCloseNess arriveTime x `compare` getCloseNess arriveTime y) busIds
   in let comesIn = getModRemiander arriveTime nextBus
       in (nextBus, comesIn)

isTimeValid :: Int -> [Int] -> Bool
isTimeValid time = all (\busId -> time `mod` busId == 0)

part1 :: IO ()
part1 = do
  lines <- getInput
  let (arriveTime, busIds) = parseInput lines
  let (nextBus, comesIn) = findNextBus arriveTime (map snd busIds)
  print (nextBus, comesIn)
  print $ nextBus * comesIn

-- Imma be real, i have no idea whats going on here
-- all i now is the first number in the tuple gives you z
-- where (z * a) `mod` b == 1
egcd :: Int -> Int -> (Int, Int, Int)
egcd a 0 = (1, 0, a)
egcd a b =
  let (q, r) = a `quotRem` b
   in let (s, t, g) = egcd b r
       in (t, s - q * t, abs g)

-- https://brilliant.org/wiki/chinese-remainder-theorem/
chineseRemainder :: [Int] -> [Int] -> Int
chineseRemainder mods remainders =
  let prod = product mods
   in let x =
            sum $
              zipWith
                ( \modulo remainder ->
                    ( let y = prod `div` modulo
                       in let (z, _, _) = egcd y modulo
                           in remainder * z * y
                    )
                )
                mods
                remainders
       in x `mod` prod

part2 :: IO ()
part2 = do
  lines <- getInput
  let (_, busIds) = parseInput lines
  let remainders = map (\(idx, x) -> x - idx) busIds
  let mods = map snd busIds
  print $ chineseRemainder mods remainders

getInput :: IO [String]
getInput = lines <$> readFile "./in"

main :: IO ()
main = part2
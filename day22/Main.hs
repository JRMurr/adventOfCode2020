module Main where

import Data.List
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

type Deck = [Int]

type Decks = (Deck, Deck)

type PrevRoundsSet = Set Decks

type AllRounds = Map Decks (Int, Deck)

mapWithIdx :: ((Int, a) -> b) -> [a] -> [b]
mapWithIdx f = zipWith (curry f) [0 ..]

playGameP1 :: Decks -> Decks
playGameP1 ([], x) = ([], x)
playGameP1 (x, []) = (x, [])
playGameP1 (d1, d2) =
  if d1H >= d2H then playGameP1 (d1T ++ [d1H, d2H], d2T) else playGameP1 (d1T, d2T ++ [d2H, d1H])
  where
    (d1H, d1T) = fromJust $ uncons d1
    (d2H, d2T) = fromJust $ uncons d2

calcScoreOfDeck :: Deck -> Int
calcScoreOfDeck d = sum $ mapWithIdx (\(idx, x) -> x * (idx + 1)) (reverse d)

calcScore :: Decks -> Int
calcScore (x, []) = calcScoreOfDeck x
calcScore ([], x) = calcScoreOfDeck x

-- return number of player who won
playGameP2 :: Decks -> PrevRoundsSet -> (Int, Deck)
playGameP2 d s | d `elem` s = (1, fst d) -- 1 wins since this setup was seen
playGameP2 (x, []) _ = (1, x)
playGameP2 ([], x) _ = (2, x)
playGameP2 (d1, d2) s = case winnerSubGame of
  Just (1, _) -> p1Wins
  Just (2, _) -> p2Wins
  _ -> if d1H >= d2H then p1Wins else p2Wins
  where
    (d1H, d1T) = fromJust $ uncons d1
    (d2H, d2T) = fromJust $ uncons d2
    winnerSubGame = if (d1H <= length d1T) && (d2H <= length d2T) then Just (playGameP2 (d1T, d2T) Set.empty) else Nothing
    p1Wins = playGameP2 (d1T ++ [d1H, d2H], d2T) (Set.insert (d1, d2) s)
    p2Wins = playGameP2 (d1T, d2T ++ [d2H, d1H]) (Set.insert (d1, d2) s)

part1 :: IO ()
part1 = do
  input <- getInput
  print $ calcScore $ playGameP1 input
  return ()

part2 :: IO ()
part2 = do
  input <- getInput
  let (_, winner) = playGameP2 input Set.empty
  print $ calcScoreOfDeck winner
  return ()

main :: IO ()
main = part2

groupByBlank :: [String] -> [[String]]
groupByBlank = splitOn [""]

tuplify2 :: [a] -> (a, a)
tuplify2 [x, y] = (x, y)

getInput :: IO Decks
getInput = tuplify2 . map (map read . tail) . groupByBlank . lines <$> readFile "./in.example"
module Main where

import Data.Foldable (Foldable (toList))
import Data.List
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

type Deck = [Int]

type DeckSeq = Seq Int

type Decks = (Deck, Deck)

type DecksSeq = (DeckSeq, DeckSeq)

type PrevRoundsSet = Set DecksSeq

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

unconsSeq :: Seq a -> (a, Seq a)
unconsSeq s = (Seq.index s 0, Seq.drop 1 s)

-- return number of player who won
playGameP2 :: DecksSeq -> PrevRoundsSet -> (Int, DeckSeq)
playGameP2 d s | d `elem` s = (1, fst d) -- 1 wins since this setup was seen
playGameP2 (x, y) _ | y == Seq.empty = (1, x)
playGameP2 (y, x) _ | y == Seq.empty = (2, x)
playGameP2 (d1, d2) s = case winnerSubGame of
  Just (1, _) -> p1Wins
  Just (2, _) -> p2Wins
  _ -> if d1H >= d2H then p1Wins else p2Wins
  where
    (d1H, d1T) = unconsSeq d1
    (d2H, d2T) = unconsSeq d2
    winnerSubGame = if (d1H <= length d1T) && (d2H <= length d2T) then Just (playGameP2 (d1T, d2T) Set.empty) else Nothing
    p1Wins = playGameP2 (d1T Seq.>< Seq.fromList [d1H, d2H], d2T) (Set.insert (d1, d2) s)
    p2Wins = playGameP2 (d1T, d2T Seq.>< Seq.fromList [d2H, d1H]) (Set.insert (d1, d2) s)

part1 :: IO ()
part1 = do
  input <- getInput
  print $ calcScore $ playGameP1 input
  return ()

part2 :: IO ()
part2 = do
  (d1, d2) <- getInput
  let (_, winner) = playGameP2 (Seq.fromList d1, Seq.fromList d2) Set.empty
  print $ calcScoreOfDeck $ toList winner
  return ()

main :: IO ()
main = part2

groupByBlank :: [String] -> [[String]]
groupByBlank = splitOn [""]

tuplify2 :: [a] -> (a, a)
tuplify2 [x, y] = (x, y)

getInput :: IO Decks
getInput = tuplify2 . map (map read . tail) . groupByBlank . lines <$> readFile "./in"
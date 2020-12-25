module Main where

import Data.Char (digitToInt)
import Data.Maybe
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Debug.Trace
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
type Circle = (Int, IntMap Int)

-- Given an index into a list/seq return the correct index with wrapping
getIndexWrapped i s =
  i `mod` length s

getElem :: Seq a -> Int -> a
getElem s i = s `Seq.index` getIndexWrapped i s

removeRange :: Seq a -> Int -> Int -> (Seq a, Seq a) -- (newList, removedElems)
removeRange s start len = Seq.foldlWithIndex foldFunc (Seq.empty, Seq.empty) s
  where
    removeIdxs = [getIndexWrapped i s | i <- [start .. (start + len -1)]]
    foldFunc (newSeq, removed) idx x = if idx `elem` removeIdxs then (newSeq, removed Seq.|> x) else (newSeq Seq.|> x, removed)

insertAt :: Int -> a -> Seq a -> Seq a
insertAt i x s | i >= length s = s Seq.|> x
insertAt i x s = Seq.insertAt i x s

insertRange :: Show a => Seq a -> Int -> Seq a -> Seq a
insertRange s idx toInsert =
  -- trace (show (idx, toInsert, s)) $
  foldl (\acc (x, i) -> insertAt i x acc) s insertWithIdx
  where
    insertWithIdx = Seq.zip toInsert $ Seq.fromList [i `mod` 9 | i <- [idx .. (idx + length toInsert -1)]]

rotate :: Int -> Seq a -> Seq a
rotate n = Seq.drop n <> Seq.take n

getDest :: Seq Int -> Int -> Int
getDest s currCup = destIdx
  where
    sortedSeq = Seq.unstableSort s
    -- since its sorted ascending get the elem to left of curr cup with wrapping
    destValue = getElem sortedSeq (fromJust (Seq.elemIndexL currCup sortedSeq) -1)
    -- index of the dest + 1 in the original list
    destIdx = fromJust (Seq.elemIndexL destValue s) + 1

-- TODO: rotate list so the curr cup is the head
-- will make every other func way easier to deal with
-- since the will always remove index 1 - 3 (inclusive)
-- dest value will need some work
-- cyclic linked list might make this way easier or way harder...
playRound :: Seq Int -> Int -> (Seq Int, Int)
playRound cups currCup =
  (nextCups, nextCurr)
  where
    currIdx = fromJust (Seq.elemIndexL currCup cups)
    (updatedCups, removed) = removeRange cups (currIdx + 1) 3
    rotatedCups = rotate currIdx updatedCups
    destIdx = getDest rotatedCups currCup
    nextCups = insertRange rotatedCups destIdx removed
    nextCurr = getElem nextCups (fromJust (Seq.elemIndexL currCup nextCups) + 1)

playNTimes :: Seq Int -> Int -> Int -> Seq Int
playNTimes s _ 0 = s
playNTimes s currCup n =
  let (newS, newCurr) = playRound s currCup
   in playNTimes newS newCurr (n -1)


-- seqToCircle:: Seq Int -> Circle
-- seqToCircle s = 

part1 :: IO ()
part1 = do
  input <- getInput
  let cups = Seq.fromList input
  print $ playNTimes cups (getElem cups 0) 100
  return ()

genCups :: Seq Int -> Seq Int
genCups cups = cups Seq.>< toAppend
  where
    startVal = maximum cups + 1
    numVal = (1000000 - startVal) + 1
    toAppend = Seq.iterateN numVal (1 +) startVal

pairs:: [a] -> [(a,a)]
pairs = zip <*> tail

getCircle:: [Int] -> Circle
getCircle lst = 
  (currentCup, c)
  where 
    currentCup = head lst
    m = IntMap.fromList (pairs (lst ++ [10 .. 1000000]))
    c = IntMap.insert 1000000 currentCup m

maxN = 1000000
playRoundCircle:: Circle -> Circle
playRoundCircle (n,m) = 
  let n1 = m IntMap.! n
      n2 = m IntMap.! n1
      n3 = m IntMap.! n2
      next = m IntMap.! n3
      descending = [n-1, n-2 .. 1] ++ [maxN, maxN-1 ..]
      small = head $ filter (`notElem` [n1, n2, n3]) descending
      m' = IntMap.union (IntMap.fromList [(n, next), (small, n1), (n3, m IntMap.! small)]) m
    in  (next, m') 


part2 :: IO ()
part2 = do
  input <- getInput
  let circle = getCircle input
  let (_, m) = iterate playRoundCircle circle !! 10000000
  let n1 = m IntMap.! 1
  let n2 = m IntMap.! n1
  print $ n1 * n2
  return ()

main :: IO ()
main = part2

getInput :: IO [Int]
getInput = map digitToInt . head . lines <$> readFile "./in"
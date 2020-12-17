module Main where

import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace

type TicketValues = [Int]

type ValueConstraint = [Int]

type AllConstraints = Map String ValueConstraint

type IndexToPossibleFields = Map Int [String]

type IndexToField = Map Int String

type TicketInfo = Map String Int

isNumValid :: AllConstraints -> Int -> Bool
isNumValid constraints num =
  not $ any (\range -> num `elem` range) allRanges
  where
    allRanges = Map.elems constraints

findInvalidValues :: AllConstraints -> TicketValues -> [Int]
findInvalidValues constraints = filter (isNumValid constraints)

filterInvalidTickets :: AllConstraints -> [TicketValues] -> [TicketValues]
filterInvalidTickets constraints tickets =
  [x | x <- tickets, null (findInvalidValues constraints x)]

part1 :: IO ()
part1 = do
  (constraints, myTicket, otherTickets) <- getInput
  print $ sum (concatMap (findInvalidValues constraints) otherTickets)

getPossibleFields :: AllConstraints -> IndexToPossibleFields -> Int -> [Int] -> IndexToPossibleFields
getPossibleFields constraints idxField idx values =
  Map.insert idx fields idxField
  where
    filteredConstraints = Map.filter (\range -> all (`elem` range) values) constraints
    fields = Map.keys filteredConstraints

getAllPossibleFields :: AllConstraints -> [TicketValues] -> IndexToPossibleFields
getAllPossibleFields constraints validTickets =
  foldl (\acc idx -> getPossibleFields constraints acc idx (getIdxInAllTickets idx)) Map.empty [0 .. (numFields -1)]
  where
    numFields = Map.size constraints
    getIdxInAllTickets idx = map (!! idx) validTickets

getIndexToField :: IndexToField -> [(Int, [String])] -> IndexToField
getIndexToField idxToField sortedPossibles =
  foldl (\acc (idx, fields) -> Map.insert idx (getFirstNewField fields acc) acc) idxToField sortedPossibles
  where
    getFirstNewField fields x = fromJust $ find (`notElem` Map.elems x) fields

getTicketInfo :: IndexToField -> TicketValues -> TicketInfo
getTicketInfo idxToField ticketVals =
  foldl (\acc (idx, val) -> Map.insert (idxToField Map.! idx) val acc) Map.empty (zip [0 ..] ticketVals)

part2 :: IO ()
part2 = do
  (constraints, myTicket, otherTickets) <- getInput
  let validTickets = filterInvalidTickets constraints otherTickets
  let idxToPossibleField = getAllPossibleFields constraints validTickets
  let pairs = Map.assocs idxToPossibleField
  let sortedByNumPossible = sortBy (\(_, x) (_, y) -> length x `compare` length y) pairs
  let idxToField = getIndexToField Map.empty sortedByNumPossible
  -- print $ sort . Map.elems $ Map.map length idxToPossibleField
  let myTicketInfo = getTicketInfo idxToField myTicket
  let departureVals = Map.elems $ Map.filterWithKey (\k _ -> "departure" `isPrefixOf` k) myTicketInfo
  print $ product departureVals
  return ()

groupByBlank :: [String] -> [[String]]
groupByBlank = splitOn [""]

getListByRange :: String -> [Int]
getListByRange str =
  let [min, max] = splitOn "-" str
   in [read min .. read max]

parseSingleConstraint :: AllConstraints -> String -> AllConstraints
parseSingleConstraint map str =
  Map.insert key (getListByRange range1 ++ getListByRange range2) map
  where
    [key, t] = splitOn ": " str
    [range1, range2] = splitOn " or " t

parseConstraints :: [String] -> AllConstraints
parseConstraints =
  foldl parseSingleConstraint Map.empty

parseTicketLine :: String -> TicketValues
parseTicketLine =
  map read . splitOn ","

getInput :: IO (AllConstraints, TicketValues, [TicketValues])
getInput = do
  [constraints, myTicket, otherTickets] <- groupByBlank . lines <$> readFile "./in"
  return (parseConstraints constraints, parseTicketLine (myTicket !! 1), map parseTicketLine (tail otherTickets))

main :: IO ()
main = part2
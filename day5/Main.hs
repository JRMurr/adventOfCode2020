module Main where
import Debug.Trace
import Data.List
import Data.Ord

getInput :: IO [String]
getInput = do
    fp <- readFile "./in"
    return (lines fp)

binaryStringParse :: [Char] -> Char -> Int
binaryStringParse lst trueChar = 
    let lstWithIdx = zip [0..] lst in
    sum [2^idx | (idx, x) <- lstWithIdx, x == trueChar]


type SeatCords = (Int, Int)
getSeatCords :: String -> SeatCords
getSeatCords line = 
    let (rowLst, seatLst) = splitAt 7 line in
    (binaryStringParse (reverse rowLst) 'B', binaryStringParse (reverse seatLst) 'R')

getSeatID :: String -> Int
getSeatID line = 
    let (rowNum, seatNum) = getSeatCords line in
    (8 * rowNum) + seatNum

findSeat :: [SeatCords] -> Int
findSeat lst =
    let sortByRow = sortBy (comparing fst) lst in
    let groupedByRow = groupBy (\(row1, _) (row2, _) -> row1 == row2) sortByRow in 
        case find (\x -> length x == 7) groupedByRow of 
            Just rowLst -> 
                let (rowNum, _) = head rowLst in
                    let missingSeat = head ([0..7] \\ [seat | (_, seat) <- rowLst]) in
                        (8 * rowNum) + missingSeat
            Nothing -> error "no row found"

part1 :: IO ()
part1 = do
    lines <- getInput
    print (maximum (map getSeatID lines))

part2 :: IO()
part2 = do
    lines <- getInput
    print (findSeat (map getSeatCords lines))

main :: IO ()
main = part2

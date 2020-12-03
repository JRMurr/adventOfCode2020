-- module Main where
import Debug.Trace

-- mark trees as true and cycle each line to inf
parseLine :: String -> [Bool]
parseLine line = cycle (map ('#' ==) line)

goInSlope :: [[Bool]] -> (Int, Int) -> (Int, Int) -> Int
goInSlope lst (xIdx, yIdx) (slopeX, slopeY) =
    if yIdx >= length lst then 0 else
        let elm = (lst !! yIdx) !! xIdx in
            let count = if elm then 1 else 0 in
                count + goInSlope lst (xIdx + slopeX, yIdx + slopeY) (slopeX, slopeY)


part1 = do
    fp <- readFile "./in"
    let input = map parseLine (lines fp)
    print (goInSlope input (0, 0) (3,1))

part2 = do
    fp <- readFile "./in"
    let input = map parseLine (lines fp)
    let slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]
    let counts = [goInSlope input (0, 0) x | x <- slopes]
    print (foldl (*) 1 counts)


main :: IO ()
main = part2
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

getInput :: IO [String]
getInput = do
    fp <- readFile "./in"
    return (lines fp)

part1 :: IO ()
part1 = do
    lines <- getInput
    let input = map parseLine lines
    print (goInSlope input (0, 0) (3,1))

part2 = do
    lines <- getInput
    let input = map parseLine lines
    let slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]
    let counts = [goInSlope input (0, 0) x | x <- slopes]
    print (product counts)


main :: IO ()
main = part2
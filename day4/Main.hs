import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Read
import Debug.Trace

getInput :: IO [String]
getInput = do
    fp <- readFile "./in"
    return (lines fp)

-- remove the blank lines to organize passports together
groupInfo :: [String] -> [String]
groupInfo lst = 
    let grouped = splitOn [""] lst in
        [unwords x | x <-grouped]

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)

type PassportMap = Map String String

toMap :: String -> PassportMap
toMap passport =
        Map.fromList (map (tuplify2 . splitOn ":") (words passport))

requiredKeys :: [String]
requiredKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
-- , "cid"


hasKeys :: PassportMap -> Bool
hasKeys pMap =
    all (`Map.member` pMap) requiredKeys

isByrValid :: String -> Bool
isByrValid val =
    case readMaybe val of
        Just valInt -> valInt >= 1920 && valInt <= 2002
        Nothing -> False

isIyrValid :: String -> Bool
isIyrValid val =
    case readMaybe val of
        Just valInt -> valInt >= 2010 && valInt <= 2020
        Nothing -> False

isEyrValid :: String -> Bool
isEyrValid val =
    case readMaybe val of
        Just valInt -> valInt >= 2020 && valInt <= 2030
        Nothing -> False

isHgtValid :: String -> Bool
isHgtValid val =
    case val of 
        -- regexs are for suckers
        [d1,d2,d3,'c','m'] -> (
            let val = (read::String->Int) [d1,d2,d3] in
                val >= 150 && val <= 193
            )
        [d1,d2,'i','n'] -> (
            let val = (read::String->Int) [d1,d2] in
                val >= 59 && val <= 76
            )
        _ -> False

isHclValid :: String -> Bool
isHclValid val =
    case val of
        ['#', c1, c2, c3, c4, c5, c6] ->
            all (\x -> x `elem` ['a'..'f'] || x `elem` ['0'..'9']) [c1, c2, c3, c4, c5, c6]
        _ -> False

isEclValid :: String -> Bool
isEclValid val =
    val `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]


isPidValid :: String -> Bool
isPidValid val =
    case length val of 
        9 -> all (`elem` ['0'..'9']) val
        _ -> False

isPairValid :: (String, String) -> Bool
isPairValid (key, val) =
    case key of 
        "byr" -> isByrValid val
        "iyr" -> isIyrValid val
        "eyr" -> isEyrValid val
        "hgt" -> isHgtValid val
        "hcl" -> isHclValid val
        "ecl" -> isEclValid val
        "pid" -> isPidValid val
        _ -> True

isValid :: PassportMap -> Bool
isValid pMap =
    all isPairValid (Map.toAscList pMap)

part1 :: IO ()
part1 = do
    lines <- getInput
    let pMaps = map toMap (groupInfo lines)
    let validMaps = [x | x <- pMaps, hasKeys x]
    print (length validMaps)

part2 :: IO ()
part2 = do
    lines <- getInput
    let pMaps = map toMap (groupInfo lines)
    let validMaps = [x | x <- pMaps, hasKeys x, isValid x]
    print (length validMaps)

main :: IO ()
main = part2
import Data.List.Split
import Data.List

data Policy = Policy {lower:: Int, upper:: Int, letter:: Char, pass:: String} deriving (Show)  
parseBounds :: String -> Maybe (Int, Int)
parseBounds str = 
    case splitOn "-" str of
        [lower, upper] -> Just (read lower, read upper)
        _ -> Nothing

parseLine :: [String] -> Policy
parseLine line =
    case line of
        [bounds, [letter,':'], pass] -> (
            case parseBounds bounds of
                Just(lower, upper) -> Policy {lower=lower, upper=upper, letter=letter, pass=pass}
                _ -> error "bad bounds"
            )
        _ -> error "bad line"

isValidPart1 :: Policy -> Bool
isValidPart1 policy =
    let count = length [x | x <- pass policy, x == letter policy] in
    count >= lower policy && count <= upper policy

isValidPart2 :: Policy -> Bool
isValidPart2 policy =
    let indices = elemIndices (letter policy) (pass policy) in
        let (lowerIdx, upperIdx) = (lower policy -1, upper policy - 1) in
            -- xor
            (lowerIdx `elem` indices) /= (upperIdx `elem` indices)



main = do
    fp <- readFile "./in"
    let input = map (parseLine . words) (lines fp)
    let validPasswords = length [x | x <- input, isValidPart2 x]
    print validPasswords
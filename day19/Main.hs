module Main where

import Data.Function
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List.Split (splitOn)

data Rule = Const Char | Seq [Int] | Alt Rule Rule deriving (Eq, Show)

type RuleMap = IntMap Rule

parseRuleValue :: String -> Rule
parseRuleValue ['"', c, '"'] = Const c
parseRuleValue str =
  let ids = splitOn " " str
   in Seq (map read ids)

parseRuleLine :: String -> (Int, Rule)
parseRuleLine str =
  (read id, finalRule)
  where
    [id, rules] = splitOn ": " str
    splitRules = map parseRuleValue $ splitOn " | " rules
    finalRule =
      if length splitRules == 1
        then head splitRules
        else let [r1, r2] = splitRules in Alt r1 r2

getAllRules :: [String] -> RuleMap
getAllRules = IntMap.fromList . map parseRuleLine

match :: RuleMap -> Rule -> String -> (String -> Bool) -> Bool
match m (Const c) (x : xs) matchFunc = (x == c) && matchFunc xs
match m (Const c) [] _ = False
match m (Seq (id : ids)) str matchFunc =
  match m (m IntMap.! id) str (\newStr -> match m (Seq ids) newStr matchFunc)
match m (Seq []) str matchFunc = matchFunc str
match m (Alt r1 r2) str matchFunc = match m r1 str matchFunc || match m r2 str matchFunc

matchLine :: RuleMap -> String -> Bool
matchLine m s =
  match m (Seq [0]) s null

part1 :: IO ()
part1 = do
  (rules, input) <- getInput
  let ruleMap = getAllRules rules
  let valid = filter (matchLine ruleMap) input
  print $ length valid
  return ()

new8 :: Rule
new8 = Alt (Seq [42]) (Seq [42, 8])

new11 :: Rule
new11 = Alt (Seq [42, 31]) (Seq [42, 11, 31])

part2 :: IO ()
part2 = do
  (rules, input) <- getInput
  let ruleMap = (IntMap.insert 8 new8) . (IntMap.insert 11 new11) $ (getAllRules rules)
  let valid = filter (matchLine ruleMap) input
  print $ length valid
  return ()

main :: IO ()
main = part2

groupByBlank :: [String] -> [[String]]
groupByBlank = splitOn [""]

tuplify2 :: [a] -> (a, a)
tuplify2 [x, y] = (x, y)

getInput :: IO ([String], [String]) -- (rules, lines)
getInput = tuplify2 . groupByBlank . lines <$> readFile "./in"
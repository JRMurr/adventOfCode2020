module Main where

import Data.List
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe

type AllergenToPossibleIngredients = Map String [String]

type AllergenToIngredient = Map String String

-- get number of times each element appears in the list
frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = Map.toList (Map.fromListWith (+) [(x, 1) | x <- xs])

getMaxFreq :: [(a, Int)] -> Int
getMaxFreq xs = snd $ maximumBy (\(_, x) (_, x') -> x `compare` x') xs

filterNonMax :: (Ord a) => [a] -> [a]
filterNonMax xs =
  mapMaybe (\(x, i) -> if i == max then Just x else Nothing) counts
  where
    counts = frequency xs
    max = getMaxFreq counts

-- used in alter call to map to add ingredients to an allergen
addIngredients :: [String] -> Maybe [String] -> Maybe [String]
addIngredients s (Just lst) = Just (s ++ lst)
addIngredients s Nothing = Just s

-- map is allergen to list of every ingredient it appeared with
makePossibilities :: [([String], [String])] -> AllergenToPossibleIngredients
makePossibilities = foldl handleRow Map.empty
  where
    handleRow m (ingredients, allergens) = foldl (\acc allergen -> Map.alter (addIngredients ingredients) allergen acc) m allergens

simplifyPossibilities :: AllergenToPossibleIngredients -> AllergenToPossibleIngredients
simplifyPossibilities = Map.map filterNonMax

part1 :: IO ()
part1 = do
  input <- getInput
  let ingredients = concatMap fst input
  let ingredientFreq = Map.fromList $ frequency ingredients
  let ingredientsSet = nub ingredients
  let simplifiedMap = simplifyPossibilities $ makePossibilities input
  let usedIngredients = nub $ concat (Map.elems simplifiedMap)
  let notUsed = ingredientsSet \\ usedIngredients
  let usedCount = sum (map (ingredientFreq Map.!) notUsed)
  print usedCount
  return ()

removeIngredient :: AllergenToPossibleIngredients -> String -> AllergenToPossibleIngredients
removeIngredient m i =
  Map.mapMaybe removeIngredients m
  where
    removeIngredients ingredients =
      let removed = delete i ingredients
       in if null removed then Nothing else Just removed

-- find the allergen to ingredient pair whose ingredients list has length 1
findMinimalValue :: AllergenToPossibleIngredients -> (String, String)
findMinimalValue m =
  head $ Map.assocs filtered
  where
    mapFunc ingredients = if length ingredients == 1 then Just (head ingredients) else Nothing
    filtered = Map.mapMaybe mapFunc m

getMap :: AllergenToPossibleIngredients -> AllergenToIngredient -> AllergenToIngredient
getMap possibleMap m | Map.null possibleMap = m
getMap possibleMap m = getMap removedMap (Map.insert allergen ingredient m)
  where
    (allergen, ingredient) = findMinimalValue possibleMap
    removedMap = removeIngredient possibleMap ingredient

part2 :: IO ()
part2 = do
  input <- getInput
  let simplifiedMap = simplifyPossibilities $ makePossibilities input
  let pairs = sortBy (\(x, _) (x', _) -> x `compare` x') $ Map.assocs $ getMap simplifiedMap Map.empty
  let allergens = map snd pairs
  putStrLn $ intercalate "," allergens
  return ()

main :: IO ()
main = part2

parseLine :: String -> ([String], [String])
parseLine str =
  (ingredients, allergens)
  where
    [ingredientsStr, allergensStr] = splitOn " (contains " str
    ingredients = words ingredientsStr
    allergens = splitOn ", " (init allergensStr)

getInput :: IO [([String], [String])]
getInput = map parseLine . lines <$> readFile "./in"
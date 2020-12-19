module Main where

part1 :: IO ()
part1 = do
  input <- getInput
  return ()

part2 :: IO ()
part2 = do
  input <- getInput
  return ()

main :: IO ()
main = part1

getInput :: IO [String]
getInput = lines <$> readFile "./in.example"
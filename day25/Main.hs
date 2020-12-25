module Main where

runStep:: Int -> Int -> Int
runStep v subjectNum = 
  (v * subjectNum) `mod` 20201227

findLoopSize:: Int -> Int -> Int -> Int -> Int
findLoopSize pubKey subjectNum n currVal = 
  if stepRes == pubKey then n else findLoopSize pubKey subjectNum (n+1) stepRes
  where
    stepRes = runStep currVal subjectNum

runSteps :: Int -> Int -> Int -> Int
runSteps 0 v _ = v
runSteps n v subjectNum = runSteps (n-1) (runStep v subjectNum) subjectNum

part1 :: IO ()
part1 = do
  input <- getInput
  let loopSizes = map (\x -> findLoopSize x 7 1 1) input
  let encryptionKey = runSteps (head loopSizes) 1 (last input)
  print $ encryptionKey
  return ()

part2 :: IO ()
part2 = do
  input <- getInput
  return ()

main :: IO ()
main = part1

getInput :: IO [Int]
getInput = map read . lines <$> readFile "./in"
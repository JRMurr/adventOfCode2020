module Main where

data Instruction
  = Nop Int
  | Acc Int
  | Jmp Int
  deriving (Show)

data ProgramStatus = Running | InfiniteLoop | Terminated deriving (Show)

data State = State {acc :: Int, instructions :: [Instruction], status :: ProgramStatus} deriving (Show)

parseInt :: String -> Int
parseInt val =
  case val of
    '+' : rest -> read rest
    '-' : rest -> (-1) * read rest

parseLine :: String -> Instruction
parseLine line =
  case words line of
    ["nop", val] -> Nop (parseInt val)
    ["acc", val] -> Acc (parseInt val)
    ["jmp", val] -> Jmp (parseInt val)
    _ -> error ("bad line: " ++ line)

initState :: Int -> [String] -> State
initState startAcc lines =
  State {acc = startAcc, instructions = map parseLine lines, status = Running}

runLine :: State -> Int -> [Int] -> State
runLine state pc linesRan = case instructions state !! pc of
  Nop _ -> runProg state (pc + 1) (pc : linesRan)
  Acc val ->
    let newAcc = acc state + val
     in runProg (state {acc = newAcc}) (pc + 1) (pc : linesRan)
  Jmp val ->
    runProg state (pc + val) (pc : linesRan)

runProg :: State -> Int -> [Int] -> State
runProg state pc linesRan
  | pc `elem` linesRan = state {status = InfiniteLoop}
  | pc >= length (instructions state) = state {status = Terminated}
  | otherwise = runLine state pc linesRan

replace :: (Eq a, Num a, Enum a) => a -> b -> [b] -> [b]
replace index elem = map (\(index', elem') -> if index' == index then elem else elem') . zip [0 ..]

changeInstruction :: State -> Int -> Instruction
changeInstruction state index =
  case instructions state !! index of
    Nop val -> Jmp val
    Jmp val -> Nop val
    Acc val -> Acc val

swapInstruction :: State -> Int -> State
swapInstruction state index =
  state {instructions = replace index (changeInstruction state index) (instructions state)}

tryChange :: [String] -> Int -> State
tryChange input index
  | index >= length input = error "no more changes possible"
  | otherwise =
    let resState = runProg (swapInstruction (initState 0 input) index) 0 []
     in case status resState of
          Terminated -> resState
          InfiniteLoop -> tryChange input (index + 1)
          _ -> error "should not be running"

part1 = do
  input <- getInput
  let state = runProg (initState 0 input) 0 []
  print state

part2 = do
  input <- getInput
  let state = tryChange input 0
  print state

getInput :: IO [String]
getInput = lines <$> readFile "./in"

main :: IO ()
main = part2

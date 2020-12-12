module Main where

import Debug.Trace

type Cord = (Int, Int) -- X,Y

data Direction = North | South | East | West deriving (Show)

data Turn = L | R deriving (Show)

data ShipInfo = ShipInfo {pos :: Cord, facing :: Direction} deriving (Show)

data Action = Direction Direction | Turn Turn | Forward deriving (Show)

type Command = (Action, Int)

type Slope = (Int, Int)

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft West = South
turnLeft South = East
turnLeft East = North

turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

parseLine :: String -> Command
parseLine line = case head line of
  'N' -> (Direction North, amount)
  'S' -> (Direction South, amount)
  'E' -> (Direction East, amount)
  'W' -> (Direction West, amount)
  'L' -> (Turn L, amount)
  'R' -> (Turn R, amount)
  'F' -> (Forward, amount)
  _ -> error "unsupported action"
  where
    amount = read (tail line)

getCommands :: [String] -> [Command]
getCommands = map parseLine

initShip :: Cord -> Direction -> ShipInfo
initShip cord dir = ShipInfo {pos = cord, facing = dir}

addSlope :: Cord -> Slope -> Cord
addSlope cord slope =
  (fst cord + fst slope, snd cord + snd slope)

getSlope :: Direction -> Int -> Slope
getSlope North amount = (0, amount)
getSlope South amount = (0, -1 * amount)
getSlope East amount = (amount, 0)
getSlope West amount = (-1 * amount, 0)

moveShip :: ShipInfo -> Slope -> ShipInfo
moveShip shipInfo slope =
  let newPos = addSlope (pos shipInfo) slope
   in shipInfo {pos = newPos}

turnShip :: ShipInfo -> Turn -> Int -> ShipInfo
turnShip s _ 0 = s
turnShip ship L times = turnShip (ship {facing = turnLeft (facing ship)}) L (times -1)
turnShip ship R times = turnShip (ship {facing = turnRight (facing ship)}) R (times -1)

rotateShip :: ShipInfo -> Turn -> Int -> ShipInfo
rotateShip ship t degrees =
  if degrees `mod` 90 /= 0
    then error "only multiples of 90"
    else
      let numTurns = degrees `div` 90
       in turnShip ship t numTurns

runCommandPart1 :: ShipInfo -> Command -> ShipInfo
runCommandPart1 shipInfo command = case command of
  (Direction d, amount) -> moveShip shipInfo (getSlope d amount)
  (Turn t, amount) -> rotateShip shipInfo t amount
  (Forward, amount) -> moveShip shipInfo (getSlope (facing shipInfo) amount)

part1 :: IO ()
part1 = do
  lines <- getInput
  let commands = getCommands lines
  let shipStart = initShip (0, 0) East
  let finalShip = foldl runCommandPart1 shipStart commands
  print finalShip
  let finalPos = pos finalShip
  print $ (abs . fst) finalPos + (abs . snd) finalPos

part2 :: IO ()
part2 = do
  lines <- getInput
  print lines

getInput :: IO [String]
getInput = lines <$> readFile "./in"

main :: IO ()
main = part1
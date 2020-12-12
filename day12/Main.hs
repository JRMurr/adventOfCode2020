module Main where

import Debug.Trace

type Cord = (Int, Int) -- X,Y

data Direction = North | South | East | West deriving (Show)

data Turn = L | R deriving (Show)

data ShipInfo = ShipInfo {pos :: Cord, facing :: Direction, wayPoint :: Slope} deriving (Show)

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

initShip :: Cord -> Direction -> Cord -> ShipInfo
initShip cord dir wp = ShipInfo {pos = cord, facing = dir, wayPoint = wp}

addSlope :: Cord -> Slope -> Cord
addSlope cord slope =
  (fst cord + fst slope, snd cord + snd slope)

subSlope :: Cord -> Slope -> Cord
subSlope cord slope = (fst cord - fst slope, snd cord - snd slope)

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

radians :: Floating a => a -> a
radians t = t * pi / 180

roundedCos :: Int -> Int
roundedCos x = (round . cos . radians) (fromIntegral x)

roundedSin :: Int -> Int
roundedSin x = (round . sin . radians) (fromIntegral x)

rotatePointAround :: Turn -> Int -> Cord -> Cord
rotatePointAround R degrees (x, y) =
  -- clockwise
  let (c, s) = (roundedCos degrees, roundedSin degrees)
   in (x * c + y * s, (-1 * x) * s + y * c)
rotatePointAround L degrees (x, y) =
  -- counter clockwise
  let (c, s) = (roundedCos degrees, roundedSin degrees)
   in (x * c - y * s, x * s + y * c)

rotateWayPoint :: ShipInfo -> Turn -> Int -> ShipInfo
rotateWayPoint shipInfo t degrees =
  let wp = wayPoint shipInfo
   in let newWp = rotatePointAround t degrees wp
       in shipInfo {wayPoint = newWp}

moveWayPoint :: ShipInfo -> Slope -> ShipInfo
moveWayPoint shipInfo slope =
  let newPos = addSlope (wayPoint shipInfo) slope
   in shipInfo {wayPoint = newPos}

multSlope :: Slope -> Int -> Slope
multSlope (x, y) amount =
  (x * amount, y * amount)

runCommandPart2 :: ShipInfo -> Command -> ShipInfo
runCommandPart2 shipInfo command = case command of
  (Direction d, amount) -> moveWayPoint shipInfo (getSlope d amount)
  (Turn t, amount) -> rotateWayPoint shipInfo t amount
  (Forward, amount) -> moveShip shipInfo (multSlope (wayPoint shipInfo) amount)

part1 :: IO ()
part1 = do
  lines <- getInput
  let commands = getCommands lines
  let shipStart = initShip (0, 0) East (0, 0)
  let finalShip = foldl runCommandPart1 shipStart commands
  print finalShip
  let finalPos = pos finalShip
  print $ (abs . fst) finalPos + (abs . snd) finalPos

part2 :: IO ()
part2 = do
  lines <- getInput
  let commands = getCommands lines
  let shipStart = initShip (0, 0) East (10, 1)
  let finalShip = foldl runCommandPart2 shipStart commands
  print finalShip
  let finalPos = pos finalShip
  print $ (abs . fst) finalPos + (abs . snd) finalPos

getInput :: IO [String]
getInput = lines <$> readFile "./in"

main :: IO ()
main = part2
module AoC2022.Day15
  ( day15'1
  , day15'2
  ) where

manhattenDistance :: (Int, Int) -> (Int, Int) -> Int
manhattenDistance (ax, ay) (bx, by) = abs (ax - bx) + abs (ay - by)

toDiags :: (Int, Int) -> (Int, Int)
toDiags (x, y) = (x - y, x + y)

fromDiags :: (Int, Int) -> (Int, Int)
fromDiags (x', y') = ((x' + y') `div` 2, (y' - x') `div` 2)

type SensorAndBeacon = ((Int, Int), (Int, Int), Int)

parseLine :: String -> SensorAndBeacon
parseLine s = do
  let w = words s
  let xS = read . init . drop 2 $ w !! 2
  let yS = read . init . drop 2 $ w !! 3
  let xB = read . init . drop 2 $ w !! 8
  let yB = read . drop 2 $ w !! 9
  let range = manhattenDistance (xS, yS) (xB, yB)
  (toDiags (xS, yS), toDiags (xB, yB), range)

parseInput :: String -> [SensorAndBeacon]
parseInput = map parseLine . lines

data Block
  = Unknown
  | Clear
  | Beacon
  | Sensor

instance Show Block where
  show a =
    case a of
      Unknown -> "?"
      Clear   -> " "
      Beacon  -> "B"
      Sensor  -> "S"

instance Eq Block where
  Unknown == Unknown = True
  Clear == Clear     = True
  Beacon == Beacon   = True
  Sensor == Sensor   = True
  _ == _             = False

impactOnY :: Int -> Int -> Int -> SensorAndBeacon -> [Block]
impactOnY minX' maxX' y ((x'S, y'S), (x'B, y'B), range) =
  [ do let y' = x' + (2 * y)
       if x' == x'S && y' == y'S
         then Sensor
         else if x' == x'B && y' == y'B
                then Beacon
                else if abs (x' - x'S) > range || abs (y' - y'S) > range
                       then Unknown
                       else Clear
  | x' <- [minX' .. maxX']
  ]

overlay :: [Block] -> [Block] -> [Block]
overlay =
  zipWith
    (\a b ->
       if a == Unknown
         then b
         else a)

blocks :: Int -> Int -> Int -> [SensorAndBeacon] -> [Block]
blocks minX' maxX' y =
  foldl overlay [Unknown | _ <- [minX' .. maxX']] .
  map (impactOnY minX' maxX' y)

-- Determines whether we're running the test or real input
isTestData :: [SensorAndBeacon] -> Bool
isTestData sensors = abs ((\(s, _, _) -> fst s) . head $ sensors) > 100

day15'1 :: String -> Int
day15'1 str = do
  let sensors = parseInput str
  let minX' = minimum . map (\(s, _, range) -> fst s - range) $ sensors
  let maxX' = maximum . map (\(s, _, range) -> fst s + range) $ sensors
  let y =
        if isTestData sensors
          then 2000000
          else 10
  length . filter (== Clear) . blocks minX' maxX' y $ sensors

-- Part 2
tuningFreq :: (Int, Int) -> Int
tuningFreq (x, y) = x * 4000000 + y

getMaxX' :: Int -> Int -> Int
getMaxX' maxY' y' = min y' (maxY' - y')

pointInSensorRange :: (Int, Int) -> SensorAndBeacon -> Bool
pointInSensorRange (x', y') ((x'S, y'S), _, range) =
  abs (x' - x'S) <= range && abs (y' - y'S) <= range

-- Search the positive x' (diagonal coords) space for an unknown space
searchPosX' ::
     (Int, Int) -> Int -> Maybe Int -> [SensorAndBeacon] -> Maybe (Int, Int)
searchPosX' (x', y') maxY' nextY' sensors =
  case filter (pointInSensorRange (x', y')) sensors of
    [] -> Just (x', y')
    ((x'S, y'S), _, range):_ ->
      (do let newX' = x'S + range + 1
          let newY' =
                case nextY' of
                  Nothing    -> y'S + range + 1
                  Just nxtY' -> min (y'S + range + 1) nxtY'
          let maxX' = getMaxX' maxY' y'
          if newX' <= maxX'
            then searchPosX' (newX', y') maxY' (Just newY') sensors
            -- TODO: Handle edge effects better... Might be a more interesting
            -- y' to go to if the gap is on an edge
            -- Turns out no need, so f*** it :)
            else if newY' <= maxY'
                   then searchPosX' (0, newY') maxY' Nothing sensors
                   else Nothing)

runSearch :: Int -> [SensorAndBeacon] -> Maybe (Int, Int)
runSearch maxY' sensors =
  case searchPosX' (0, 0) maxY' Nothing sensors of
    Just coords -> Just coords
    Nothing
      -- Flip the X coords and search again.
     ->
      case searchPosX' (0, 0) maxY' Nothing .
           map
             (\((x'S, y'S), (x'B, y'B), range) ->
                ((-x'S, y'S), (-x'B, y'B), range)) $
           sensors of
        Just (x', y') -> Just (-x', y')
        Nothing       -> Nothing

day15'2 :: String -> Int
day15'2 str = do
  let sensors = parseInput str
  let maxY' =
        2 *
        if isTestData sensors
          then 4000000
          else 20
  case runSearch maxY' sensors of
    Just coords' -> tuningFreq . fromDiags $ coords'
    Nothing      -> -1

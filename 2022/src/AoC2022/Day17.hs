module AoC2022.Day17
  ( day17'1
  , day17'2
  ) where

import           Data.List   (elemIndex)
import           Debug.Trace (trace)
import           Util        (every)

data Block
  = Air
  | Rock

data Dir
  = L
  | R

instance Show Block where
  show a =
    case a of
      Air  -> "."
      Rock -> "\x2588"

instance Eq Block where
  Air == Air   = True
  Rock == Rock = True
  _ == _       = False

rockShapes :: [[[Block]]]
rockShapes =
  [ [[Rock, Rock, Rock, Rock]]
  , [[Air, Rock, Air], [Rock, Rock, Rock], [Air, Rock, Air]]
  , [[Air, Air, Rock], [Air, Air, Rock], [Rock, Rock, Rock]]
  , [[Rock], [Rock], [Rock], [Rock]]
  , [[Rock, Rock], [Rock, Rock]]
  ]

shiftL :: [[Block]] -> [[Block]]
shiftL blocks =
  if any (\row -> head row == Rock) blocks
    then blocks
    else map (\row -> tail row ++ [Air]) blocks

shiftR :: [[Block]] -> [[Block]]
shiftR blocks =
  if any (\row -> last row == Rock) blocks
    then blocks
    else map (\row -> Air : init row) blocks

shift :: Dir -> [[Block]] -> [[Block]]
shift dir =
  case dir of
    L -> shiftL
    R -> shiftR

shiftDown :: [[Block]] -> [[Block]]
shiftDown bs = emptyRow : init bs

accessibleBlocks :: [Block] -> [[Block]] -> [[Block]]
accessibleBlocks row followingRows =
  case followingRows of
    [] -> []
    nextrow:morerows -> do
      let direct =
            zipWith
              (\c n ->
                 if c == Air && n == Air
                   then Air
                   else Rock)
              row
              nextrow
      let indirect =
            zipWith
              (\i n ->
                 if n /= Rock &&
                    Air `elem`
                    [ direct !! i
                    , direct !! max 0 (i - 1)
                    , direct !! min 6 (i + 1)
                    ]
                   then Air
                   else Rock)
              [0 ..]
              nextrow
      if Air `elem` indirect
        then trimLeadingAir $ indirect : accessibleBlocks indirect morerows
        else []

data Record =
  Record
    { rIdx       :: Int
    , dIdx       :: Int
    , accessible :: [[Block]]
    }

instance Eq Record where
  a == b =
    (rIdx a == rIdx b) && (dIdx a == dIdx b) && (accessible a == accessible b)

data Model =
  Model
    { free         :: [[Block]]
    , fixed        :: [[Block]]
    , rockShapeIdx :: Int
    , dirIdx       :: Int
    , dirs         :: [Dir]
    , records      :: [Record]
    , heights      :: [Int]
    }

instance Show Model where
  show m =
    concatMap
      (\rowN ->
         '\n' :
         concatMap
           (\colN ->
              if (free m !! rowN) !! colN == Air
                then show ((fixed m !! rowN) !! colN)
                else map
                       (\c ->
                          if c == '\x2588'
                            then '@'
                            else c) $
                     show ((free m !! rowN) !! colN))
           [0 .. length (fixed m !! rowN) - 1]) $
    [0 .. length (fixed m) - 1]

initialModel :: [Dir] -> Model
initialModel inputDirs =
  Model
    { free = []
    , fixed = [emptyRow]
    , rockShapeIdx = 0
    , dirIdx = 0
    , dirs = inputDirs
    , records = []
    , heights = []
    }

trimLeadingAir :: [[Block]] -> [[Block]]
trimLeadingAir region =
  case region of
    [] -> region
    row:rest ->
      if any (/= Air) row
        then region
        else trimLeadingAir rest

emptyRow :: [Block]
emptyRow = [Air | _ <- [1 .. 7 :: Int]]

startNextRockShape :: Model -> Model
startNextRockShape model = do
  let rockShape = rockShapes !! rockShapeIdx model
  let trimmedFixed = trimLeadingAir $ fixed model
  let rockRows = map (\row -> take 7 $ [Air, Air] ++ row ++ emptyRow) rockShape
  let newFree = rockRows ++ [emptyRow | _ <- [1 .. length trimmedFixed + 3]]
  let newFixed = [emptyRow | _ <- [1 .. length rockRows + 3]] ++ trimmedFixed
  model
    { free = newFree
    , fixed = newFixed
    , rockShapeIdx = (rockShapeIdx model + 1) `mod` length rockShapes
    }

clash :: [[Block]] -> [[Block]] -> Bool
clash as bs =
  any (\(arow, brow) -> any (\(a, b) -> a == Rock && b == Rock) $ zip arow brow) $
  zip as bs

merge :: [[Block]] -> [[Block]] -> [[Block]]
merge =
  zipWith
    (zipWith
       (\a b ->
          if a == Rock || b == Rock
            then Rock
            else Air))

dropFrom :: [[Block]] -> Model -> Model
dropFrom freeBlocks model = do
  let droppedFree = shiftDown freeBlocks
  if any (/= Air) (last freeBlocks) || clash droppedFree (fixed model)
    then model
           { fixed = merge freeBlocks (fixed model)
           , free = [emptyRow | _ <- [1 .. length (fixed model)]]
           }
    else runTillFixed model {free = droppedFree}

runTillFixed :: Model -> Model
runTillFixed model = do
  let dir = dirs model !! dirIdx model
  let shiftedFree = shift dir (free model)
  dropFrom
    (if clash shiftedFree (fixed model)
       then free model
       else shiftedFree)
    (model {dirIdx = (dirIdx model + 1) `mod` length (dirs model)})

height :: Model -> Int
height = length . trimLeadingAir . fixed

addRecord :: Model -> Model
addRecord model =
  model
    { records =
        Record
          { rIdx = rockShapeIdx model
          , dIdx = dirIdx model
          , accessible = accessibleBlocks emptyRow (fixed model)
          } :
        records model
    , heights = height model : heights model
    }

run :: Int -> Model -> Int
run limit model =
  if length (records model) >= limit
    then height model
    else do
      let new = addRecord . runTillFixed . startNextRockShape $ model
      let recordsToCheck = every (length rockShapes) (records model)
      case head (records new) `elemIndex` recordsToCheck of
        Just prevIdx
        -- Have seen this same state before!
         -> do
          let currRockNum = length (records new)
          let loopLen = (prevIdx + 1) * length rockShapes
          let rocksRemaining = limit - currRockNum
          let loopsRemaining = rocksRemaining `div` loopLen
          let extraStepsAfterLoop = rocksRemaining `mod` loopLen
          let currHeight = head (heights new)
          let prevHeight = heights new !! loopLen
          let loopHeight = currHeight - prevHeight
          let heightFromLoops = currHeight + (loopsRemaining * loopHeight)
          let heightFromExtraSteps =
                (heights new !! (loopLen - extraStepsAfterLoop)) - prevHeight
          trace
            ("LOOP of length " ++
             show loopLen ++
             " (" ++
             show loopsRemaining ++
             "/" ++
             show extraStepsAfterLoop ++
             ")" ++
             " has height " ++
             show loopHeight ++
             " implying height of " ++
             show (heightFromLoops + heightFromExtraSteps)) $
            heightFromLoops + heightFromExtraSteps
        Nothing -> run limit new

parseInput :: String -> [Dir]
parseInput =
  map
    (\c ->
       if c == '<'
         then L
         else R)

day17'1 :: String -> Int
day17'1 = run 2022 . initialModel . parseInput

day17'2 :: String -> Int
day17'2 = run 1000000000000 . initialModel . parseInput

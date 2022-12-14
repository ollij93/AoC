module AoC2022.Day14
  ( day14'1
  , day14'2
  , day14'2'simulation
  ) where

import           Data.List       (transpose)
import           Data.List.Split (splitOn)
import           Debug.Trace     (trace)
import           Util            (mapTuple, splitOnce)

-- Parsing the input
parsePair :: String -> (Int, Int)
parsePair = mapTuple read . splitOnce ','

parseLine :: String -> [(Int, Int)]
parseLine = map parsePair . splitOn " -> "

parseInput :: String -> [[(Int, Int)]]
parseInput = map parseLine . lines

startEndPairs :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
startEndPairs l =
  case l of
    [] -> []
    a:ls ->
      case ls of
        []  -> []
        b:_ -> (a, b) : startEndPairs ls

rockLayout :: ((Int, Int), (Int, Int)) -> [[Bool]]
rockLayout ((startx, starty), (endx, endy)) =
  [ [ (startx == endx &&
       x == startx && ((y <= starty && y >= endy) || (y <= endy && y >= starty))) ||
  (starty == endy &&
   y == starty && ((x <= startx && x >= endx) || (x <= endx && x >= startx)))
  | x <- [450 .. 549]
  ]
  | y <- [0 .. 199]
  ]

overlayRocks :: [[Bool]] -> [[Bool]] -> [[Bool]]
overlayRocks = zipWith (zipWith (||))

data Block
  = Air
  | Rock
  | Sand

instance Show Block where
  show b =
    case b of
      Air  -> " "
      Rock -> "\x2588"
      Sand -> "o"

instance Eq Block where
  Air == Air   = True
  Rock == Rock = True
  Sand == Sand = True
  _ == _       = False

toBlocks :: [[Bool]] -> [[Block]]
toBlocks =
  map
    (map
       (\b ->
          if b
            then Rock
            else Air))
  -- Read input and get block content

parseBlocks :: String -> [[Block]]
parseBlocks =
  toBlocks .
  foldl
    overlayRocks
    [[False | _ <- [450 .. 549 :: Int]] | _ <- [0 .. 199 :: Int]] .
  map rockLayout . concatMap startEndPairs . parseInput

dropLeadingEmpty :: [[Block]] -> [[Block]]
dropLeadingEmpty l =
  case l of
    [] -> []
    x:xs ->
      if any (/= Air) x
        then l
        else dropLeadingEmpty xs

trimEmpty :: [[Block]] -> [[Block]]
trimEmpty =
  transpose .
  -- Remove the empty space to the side
  (dropLeadingEmpty . reverse . dropLeadingEmpty . reverse) .
  transpose .
  -- Remove the empty space below
  (reverse . dropLeadingEmpty . reverse)

isClear :: [[Block]] -> Int -> Int -> Bool
isClear blocks x y = (blocks !! y) !! x == Air

set :: Int -> Int -> Block -> [[Block]] -> [[Block]]
set x y block blocks =
  [ [ if x == x' && y == y'
    then block
    else (blocks !! y') !! x'
  | x' <- [0 .. length (head blocks) - 1]
  ]
  | y' <- [0 .. length blocks - 1]
  ]

fallFrom ::
     (Int -> Int -> [[Block]] -> Bool)
  -> Int
  -> Int
  -> [[Block]]
  -> Maybe [[Block]]
fallFrom stopCond x y blocks
  | stopCond x y blocks = Nothing
  | isClear blocks x (y + 1) = fallFrom stopCond x (y + 1) blocks
  | isClear blocks (x - 1) (y + 1) = fallFrom stopCond (x - 1) (y + 1) blocks
  | isClear blocks (x + 1) (y + 1) = fallFrom stopCond (x + 1) (y + 1) blocks
  | otherwise = Just $ set x y Sand blocks

-- Debug methods
traceBlocks :: [[Block]] -> [[Block]]
traceBlocks blocks =
  trace
    (drop 1 . concatMap (\l -> '\n' : concatMap show l) . trimEmpty $ blocks)
    blocks

-- Solving methods
run :: (Int -> Int -> [[Block]] -> Bool) -> Int -> Int -> [[Block]] -> Int
run stopCond emitterXPos stepCount blocks =
  case fallFrom stopCond emitterXPos 0 blocks of
    Nothing   -> stepCount
    Just blks -> run stopCond emitterXPos (stepCount + 1) blks

day14'1 :: String -> Int
day14'1 =
  run (\_ y blocks -> y >= length blocks - 1) 50 0 . parseBlocks

day14'2'simulation :: String -> Int
day14'2'simulation =
  (\inp -> do
     let emit = length (head inp) `div` 2
     run (\_ _ blocks -> head blocks !! emit == Sand) emit 0 inp) .
  -- Add in the floor
  (\bs -> bs ++ [[Rock | _ <- [0 .. length (head bs) - 1]]]) .
  (\bs -> bs ++ [[Air | _ <- [0 .. length (head bs) - 1]]]) .
  -- Extend rows to be at least twice as high as the
  (\bs ->
     map
       (\l ->
          [Air | _ <- [0 .. (2 * length bs - length l) `div` 2]] ++
          l ++ [Air | _ <- [0 .. (2 * length bs - length l) `div` 2]])
       bs) .
  -- Trim trailing empty rows
  reverse . dropLeadingEmpty . reverse . parseBlocks

fillFromAbove :: [[Block]] -> [Block] -> [[Block]]
fillFromAbove above line =
  above ++
  [ [ if x <= 0 || x >= length line - 1 || line !! x /= Air
      then line !! x
      else do
        let lineabove = last above
        if Sand `elem`
           [lineabove !! (x - 1), lineabove !! x, lineabove !! (x + 1)]
          then Sand
          else Air
    | x <- [0 .. length line - 1]
    ]
  ]

day14'2 :: String -> Int
day14'2 =
  length .
  filter (== Sand) .
  concat .
  (\bs ->
     foldl
       fillFromAbove
       [ [ if x == length (head bs) `div` 2
           then Sand
           else Air
         | x <- [0 .. length (head bs) - 1]
         ]
       ]
       (tail bs)) .
  -- Add in the floor
  (\bs -> bs ++ [[Rock | _ <- [0 .. length (head bs) - 1]]]) .
  (\bs -> bs ++ [[Air | _ <- [0 .. length (head bs) - 1]]]) .
  -- Extend rows to be at least twice as high as the
  (\bs ->
     map
       (\l ->
          [Air | _ <- [0 .. 10 + ((2 * length bs) - length l) `div` 2]] ++
          l ++ [Air | _ <- [0 .. 10 + ((2 * length bs) - length l) `div` 2]])
       bs) .
  -- Trim trailing empty rows
  reverse . dropLeadingEmpty . reverse . parseBlocks

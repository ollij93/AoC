module AoC2022.Day12
  ( day12'1
  , day12'2
  ) where

import           Data.Char (ord)
import           Util      (spacepad)

parseHeights :: String -> [[Int]]
parseHeights =
  map
    (map
       (\x ->
          case x of
            'S' -> 0
            'E' -> 25
            _   -> ord x - ord 'a')) .
  lines

data Model =
  Model
    { start   :: (Int, Int)
    , end     :: (Int, Int)
    , costs   :: [[Int]]
    , heights :: [[Int]]
    }

instance Show Model where
  show model =
    concatMap ((:) '\n' . concatMap ((:) ' ' . spacepad 4 . show)) $
    costs model ++ [[]] ++ heights model

startingState :: (Char -> Bool) -> String -> Model
startingState isStartPoint s = do
  let lns = lines s
  let height = length lns
  let width = length . head $ lns
  let startingCosts =
        map
          (map
             (\x ->
                if isStartPoint x
                  then 0
                  else 999 -- Assuming the number of steps is less than this
              ))
          lns
  foldl
    (\mdl y ->
       foldl
         (\model x ->
            case (lns !! y) !! x of
              'S' -> model {start = (x, y)}
              'E' -> model {end = (x, y)}
              _   -> model)
         mdl
         [0 .. width - 1])
    Model
      { start = (0, 0)
      , end = (0, 0)
      , costs = startingCosts
      , heights = parseHeights s
      }
    [0 .. height - 1]

pointsToConsider :: Int -> Int -> Int -> Int -> [(Int, Int)]
pointsToConsider x y xmax ymax =
  filter (\(x', y') -> x' >= 0 && x' < xmax && y' >= 0 && y' < ymax) $
  map (\(dx, dy) -> (dx + x, dy + y)) [(1, 0), (-1, 0), (0, 1), (0, -1)]

canStep :: Int -> Int -> Int -> [[Int]] -> [[Int]] -> Bool
canStep x y stepNum csts hgts = do
  let hgt = (hgts !! y) !! x
  let xmax = length . head $ csts
  let ymax = length csts
  any
    (\(x'', y'') ->
       (((hgts !! y'') !! x'') >= hgt - 1) &&
       (((csts !! y'') !! x'') == stepNum)) $
    pointsToConsider x y xmax ymax

processStep :: Int -> Model -> Model
processStep stepNum model = do
  let hgts = heights model
  let csts = costs model
  let newcosts =
        map
          (\y ->
             map
               (\x -> do
                  let cst = (csts !! y) !! x
                  if cst <= stepNum
                    then cst
                    else if canStep x y stepNum csts hgts
                           then stepNum + 1
                           else cst)
               [0 .. length (head csts) - 1])
          [0 .. length csts - 1]
  model {costs = newcosts}

run :: Int -> Model -> Model
run stepNum model = do
  let (ex, ey) = end model
  let done = (costs model !! ey) !! ex < stepNum
  if done
    then model
    else run (stepNum + 1) . processStep stepNum $ model

endval :: Model -> Int
endval model = do
  let csts = costs model
  let (x, y) = end model
  (csts !! y) !! x

day12'1 :: String -> Int
day12'1 = endval . run 0 . startingState (== 'S')

day12'2 :: String -> Int
day12'2 = endval . run 0 . startingState (\c -> c == 'S' || c == 'a')

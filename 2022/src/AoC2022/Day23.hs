module AoC2022.Day23
  ( day23'1
  , day23'2
  ) where

import           Data.HashMap (Map)
import qualified Data.HashMap as Map
import           Data.List    (sort)
import           Data.Maybe   (fromMaybe)
import           Data.Set     (Set)
import qualified Data.Set     as Set

type Point = (Int, Int)

parseLine :: String -> [Int]
parseLine = map fst . filter ((== '#') . snd) . zip [0 ..]

parseInput :: String -> Set Point
parseInput =
  Set.fromList .
  concat . zipWith (\y l -> map (\x -> (x, y)) $ parseLine l) [0 ..] . lines

data Dir
  = N
  | S
  | W
  | E

dirsFromTurnNum :: Int -> [Dir]
dirsFromTurnNum n = drop (n `mod` 4) $ cycle [N, S, W, E]

data Model =
  Model
    { elfs    :: Set Point
    , turnNum :: Int
    }

initialModel :: Set Point -> Model
initialModel points = Model {elfs = points, turnNum = 0}

pointsToConsider :: Point -> Dir -> Set Point
pointsToConsider (x, y) d =
  case d of
    N -> Set.fromList [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1)]
    S -> Set.fromList [(x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]
    W -> Set.fromList [(x - 1, y - 1), (x - 1, y), (x - 1, y + 1)]
    E -> Set.fromList [(x + 1, y - 1), (x + 1, y), (x + 1, y + 1)]

pointFromDir :: Dir -> Point -> Point
pointFromDir d (x, y) =
  case d of
    N -> (x, y - 1)
    S -> (x, y + 1)
    W -> (x - 1, y)
    E -> (x + 1, y)

makeConsideration :: Model -> Point -> Maybe Point
makeConsideration model p
  | Set.null $ allIntersections = Nothing
  | Set.null $ head intersections = Just $ pointFromDir (head dirs) p
  | Set.null $ intersections !! 1 = Just $ pointFromDir (dirs !! 1) p
  | Set.null $ intersections !! 2 = Just $ pointFromDir (dirs !! 2) p
  | Set.null $ intersections !! 3 = Just $ pointFromDir (dirs !! 3) p
  | otherwise = Nothing
  where
    dirs = dirsFromTurnNum (turnNum model)
    pointsConsidered = map (pointsToConsider p) dirs
    intersections = map (Set.intersection (elfs model)) pointsConsidered
    (x, y) = p
    allIntersections =
      Set.intersection
        (elfs model)
        (Set.fromList
           [ (x - 1, y - 1)
           , (x, y - 1)
           , (x + 1, y - 1)
           , (x - 1, y)
           , (x + 1, y)
           , (x - 1, y + 1)
           , (x, y + 1)
           , (x + 1, y + 1)
           ])

makeConsiderations :: Model -> Map Point (Set Point)
makeConsiderations model =
  Set.fold
    (\p mp ->
       case makeConsideration model p of
         Just p' -> Map.insertWith Set.union p' (Set.singleton p) mp
         Nothing -> mp)
    Map.empty
    (elfs model)

uniqueConsiderations :: Map Point (Set Point) -> Map Point Point
uniqueConsiderations =
  Map.foldWithKey
    (\p' ps mp ->
       if Set.size ps == 1
         then Map.insert (head (Set.toList ps)) p' mp
         else mp)
    Map.empty

turn :: Model -> (Bool, Model)
turn model =
  ( Map.null moves
  , model
      { turnNum = turnNum model + 1
      , elfs = Set.map (\p -> fromMaybe p (Map.lookup p moves)) (elfs model)
      })
  where
    moves = uniqueConsiderations $ makeConsiderations model

takeNTurns :: Int -> Model -> Model
takeNTurns n model = iterate (snd . turn) model !! n

simTillDone :: Int -> Model -> (Int, Model)
simTillDone trn model =
  if done
    then (trn, newModel)
    else simTillDone (trn + 1) newModel
  where
    (done, newModel) = turn model

bounds :: Set Point -> ((Int, Int), (Int, Int))
bounds points = ((xMin, yMin), (xMax, yMax))
  where
    xMin = Set.findMin . Set.map fst $ points
    xMax = Set.findMax . Set.map fst $ points
    yMin = Set.findMin . Set.map snd $ points
    yMax = Set.findMax . Set.map snd $ points

getEmptySquares :: Model -> Int
getEmptySquares model =
  ((1 + xMax - xMin) * (1 + yMax - yMin)) - Set.size points
  where
    points = elfs model
    ((xMin, yMin), (xMax, yMax)) = bounds points

draw :: Model -> String
draw model =
  foldl
    (\ret y ->
       ret ++
       "\n" ++
       foldl
         (\ln x ->
            ln ++
            if Set.member (x, y) points
              then "#"
              else ".")
         ""
         [xMin .. xMax])
    ""
    [yMin .. yMax]
  where
    points = elfs model
    ((xMin, yMin), (xMax, yMax)) = bounds points

day23'1 :: String -> Int
day23'1 = getEmptySquares . takeNTurns 10 . initialModel . parseInput

day23'2 :: String -> Int
day23'2 = fst . simTillDone 1 . initialModel . parseInput

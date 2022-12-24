module AoC2022.Day24
  ( day24'1
  , day24'2
  ) where

import qualified Data.HashMap as Map
import           Data.Maybe   (fromMaybe, mapMaybe)
import           Data.Set     (Set)
import qualified Data.Set     as Set

data Dir
  = N
  | S
  | W
  | E
  deriving (Show, Eq, Ord)

data Blizzard =
  Blizzard
    { point :: (Int, Int)
    , dir   :: Dir
    }
  deriving (Show, Eq, Ord)

parseDir :: Char -> Maybe Dir
parseDir c =
  case c of
    '^' -> Just N
    'v' -> Just S
    '>' -> Just E
    '<' -> Just W
    _   -> Nothing

parseLine :: String -> [(Int, Dir)]
parseLine =
  mapMaybe
    (\(i, c) ->
       case parseDir c of
         Just d  -> Just (i, d)
         Nothing -> Nothing) .
  zip [0 ..]

parseInput :: String -> Set Blizzard
parseInput =
  Set.fromList .
  concat .
  zipWith
    (\y l -> map (\(x, d) -> Blizzard {point = (x, y), dir = d}) $ parseLine l)
    [0 ..] .
  lines

data Model =
  Model
    { blizzards   :: Set Blizzard
    , positions   :: Set (Int, Int)
    , upperLimits :: (Int, Int)
    }

instance Show Model where
  show m =
    "\n" ++
    foldl
      (\ret y ->
         ret ++
         "\n" ++
         foldl
           (\l x -> l ++ [fromMaybe '.' (Map.lookup (x, y) mp)])
           "#"
           [1 .. xMax] ++
         "#")
      (replicate (yMax + 4) '#')
      [1 .. yMax] ++
    "\n" ++ replicate (yMax + 4) '#'
    where
      (xMax, yMax) = upperLimits m
      mp =
        Set.fold
          (`Map.insert` '?')
          (Set.fold
             (\blz mp' ->
                Map.insertWith
                  (\new old ->
                     case old of
                       '^' -> '2'
                       'v' -> '2'
                       '>' -> '2'
                       '<' -> '2'
                       n   -> last . show . (+ 1) . read $ [n])
                  (point blz)
                  (case dir blz of
                     N -> '^'
                     S -> 'v'
                     E -> '>'
                     W -> '<')
                  mp')
             Map.empty
             (blizzards m))
          (positions m)

initialModel :: String -> Model
initialModel s =
  Model
    { blizzards = parseInput s
    , positions = Set.insert (1, 0) Set.empty
    , upperLimits = (length (head (lines s)) - 2, length (lines s) - 2)
    }

updateBlizzard :: (Int, Int) -> Blizzard -> Blizzard
updateBlizzard (xMax, yMax) Blizzard {point = (x, y), dir = d} =
  Blizzard {point = (x'', y''), dir = d}
  where
    (x', y') =
      case d of
        N -> (x, y - 1)
        S -> (x, y + 1)
        E -> (x + 1, y)
        W -> (x - 1, y)
    x'' = ((x' - 1) `mod` xMax) + 1
    y'' = ((y' - 1) `mod` yMax) + 1

updateBlizzards :: Model -> Set Blizzard
updateBlizzards model =
  Set.map (updateBlizzard (upperLimits model)) (blizzards model)

updatePosition :: (Int, Int) -> Set Blizzard -> (Int, Int) -> Set (Int, Int)
updatePosition (xMax, yMax) blizzs (x, y) = Set.fromList filteredPoints
    -- points where blizzards are
  where
    bpoints = Set.map point blizzs
    -- All points we could move to or wait at
    allpoints = [(x, y - 1), (x - 1, y), (x, y), (x + 1, y), (x, y + 1)]
    -- The actual set of points that can be reached
    filteredPoints =
      filter
        (\(x', y') ->
           x' > 0 &&
           y' > 0 &&
           x' <= xMax && y' <= yMax && not ((x', y') `Set.member` bpoints) ||
           (x == 1 && y == 0) || -- Special case for the starting point
           (x == xMax && y == yMax + 1) -- Special case for the end point
         )
        allpoints

update :: Model -> Model
update model = model {blizzards = blizzs, positions = poses}
  where
    blizzs = updateBlizzards model
    poses =
      Set.unions . Set.map (updatePosition (upperLimits model) blizzs) $
      positions model

run' :: Int -> Model -> (Int, Model)
run' curr model =
  if (xGoal, yGoal) `Set.member` positions newModel ||
     Set.null (positions newModel)
    -- Can move out of the valley next minute, so +1
    then (curr + 1, update newModel)
    else run' (curr + 1) newModel
  where
    newModel = update model
    (xGoal, yGoal) = upperLimits model

run :: Int -> Model -> (Int, Model)
run n model = run' n model {positions = Set.insert (1, 0) Set.empty}

day24'1 :: String -> Int
day24'1 = fst . run 1 . initialModel

runBack' :: Int -> Model -> (Int, Model)
runBack' curr model =
  if (xGoal, yGoal) `Set.member` positions newModel ||
     Set.null (positions newModel)
    -- Can move out of the valley next minute, so +1
    then (curr + 1, update newModel)
    else runBack' (curr + 1) newModel
  where
    newModel = update model
    (xGoal, yGoal) = (1, 1)

runBack :: Int -> Model -> (Int, Model)
runBack n model = runBack' n model {positions = Set.insert (x, y + 1) Set.empty}
  where
    (x, y) = upperLimits model

day24'2 :: String -> Int
day24'2 =
  fst .
  (\(n, model) -> run (n + 1) model) .
  (\(n, model) -> runBack (n + 1) model) . run 1 . initialModel

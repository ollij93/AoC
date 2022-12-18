module AoC2022.Day18
  ( day18'1
  , day18'2
  ) where

import           Data.List       (intersect, nub, sort, (\\))
import           Data.List.Split (splitOn)

type Point = (Int, Int, Int)

parsePoint :: String -> Point
parsePoint = (\l -> (head l, l !! 1, l !! 2)) . map read . splitOn ","

parseInput :: String -> [Point]
parseInput = sort . map parsePoint . lines

dirSurface :: Point -> [Point] -> Int
dirSurface (dx, dy, dz) points =
  length points -
  length
    (points `intersect` map (\(x, y, z) -> (x + dx, y + dy, z + dz)) points)

surfaceArea :: [Point] -> Int
surfaceArea points =
  dirSurface (1, 0, 0) points + dirSurface (0, 1, 0) points +
  dirSurface (0, 0, 1) points +
  dirSurface (-1, 0, 0) points +
  dirSurface (0, -1, 0) points +
  dirSurface (0, 0, -1) points

day18'1 :: String -> Int
day18'1 = surfaceArea . parseInput

expandSteam :: [Point] -> [Point] -> [Point]
expandSteam steamPoints rockPoints = do
  let xMin = minimum . map (\(x, _, _) -> x) $ rockPoints
  let xMax = maximum . map (\(x, _, _) -> x) $ rockPoints
  let yMin = minimum . map (\(_, y, _) -> y) $ rockPoints
  let yMax = maximum . map (\(_, y, _) -> y) $ rockPoints
  let zMin = minimum . map (\(_, _, z) -> z) $ rockPoints
  let zMax = maximum . map (\(_, _, z) -> z) $ rockPoints
  let expanded =
        (sort .
         filter
           (\(x, y, z) ->
              x >= xMin &&
              x <= xMax && y >= yMin && y <= yMax && z >= zMin && z <= zMax) .
         nub .
         concatMap
           (\(x, y, z) ->
              [ (x + 1, y, z)
              , (x - 1, y, z)
              , (x, y + 1, z)
              , (x, y - 1, z)
              , (x, y, z + 1)
              , (x, y, z - 1)
              , (x, y, z)
              ]) $
         steamPoints) \\
        rockPoints
  if expanded == sort steamPoints
    then expanded
    else expandSteam expanded rockPoints

day18'2 :: String -> Int
day18'2 str = do
  let points = parseInput str
  let xMin = minimum . map (\(x, _, _) -> x) $ points
  let xMax = maximum . map (\(x, _, _) -> x) $ points
  let yMin = minimum . map (\(_, y, _) -> y) $ points
  let yMax = maximum . map (\(_, y, _) -> y) $ points
  let zMin = minimum . map (\(_, _, z) -> z) $ points
  let zMax = maximum . map (\(_, _, z) -> z) $ points
  let allPoints =
        [ (x, y, z)
        | x <- [xMin .. xMax]
        , y <- [yMin .. yMax]
        , z <- [zMin .. zMax]
        ]
  let surfacePoints =
        filter
          (\(x, y, z) ->
             x == xMin ||
             x == xMax || y == yMin || y == yMax || z == zMin || z == zMax)
          allPoints
  let steamPoints = surfacePoints \\ points
  let expanded = expandSteam steamPoints points
  let internalPoints = allPoints \\ expanded
  surfaceArea internalPoints

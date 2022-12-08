module AoC2022.Day8
  ( day8'1
  , day8'2
  ) where

import           Data.Char   (ord)
import           Data.List   (transpose)

parseInput :: String -> [[Int]]
parseInput = map (map (\c -> ord c - ord '0')) . lines

visibleFromRight' :: [Int] -> [Bool]
visibleFromRight' =
  snd .
  foldr
    (\h (maxh, bs) ->
       if h > maxh
         then (h, True : bs)
         else (maxh, False : bs))
    (-1, [])

visibleFromRight :: [[Int]] -> [[Bool]]
visibleFromRight = map visibleFromRight'

visibleFromLeft :: [[Int]] -> [[Bool]]
visibleFromLeft = map reverse . visibleFromRight . map reverse

joinMaps :: (a -> a -> b) -> [[a]] -> [[a]] -> [[b]]
joinMaps fnc = zipWith (zipWith fnc)

visibleFromSides :: [[Int]] -> [[Bool]]
visibleFromSides s = joinMaps (||) (visibleFromRight s) (visibleFromLeft s)

visibleFromFrontOrBack :: [[Int]] -> [[Bool]]
visibleFromFrontOrBack = transpose . visibleFromSides . transpose

visible :: [[Int]] -> [[Bool]]
visible s = joinMaps (||) (visibleFromSides s) (visibleFromFrontOrBack s)

day8'1 :: String -> Int
day8'1 = length . filter id . concat . visible . parseInput

heightCanSee :: Int -> [Int] -> Int
heightCanSee h =
  fst .
  foldl
    (\(n, stop) t ->
       if stop
         then (n, stop)
         else (n + 1, t >= h))
    (0, False)

scenicRight'' :: Int -> [Int] -> [Int] -> ([Int], [Int])
scenicRight'' h scores hs = (heightCanSee h hs : scores, h : hs)

scenicRight' :: [Int] -> [Int]
scenicRight' =
  fst . foldr (\h (scores, hs) -> scenicRight'' h scores hs) ([], [])

scenicRight :: [[Int]] -> [[Int]]
scenicRight = map scenicRight'

scenicLeft :: [[Int]] -> [[Int]]
scenicLeft = map reverse . scenicRight . map reverse

scenicSides :: [[Int]] -> [[Int]]
scenicSides s = joinMaps (*) (scenicRight s) (scenicLeft s)

scenicFrontOrBack :: [[Int]] -> [[Int]]
scenicFrontOrBack = transpose . scenicSides . transpose

scenic :: [[Int]] -> [[Int]]
scenic s = joinMaps (*) (scenicSides s) (scenicFrontOrBack s)

day8'2 :: String -> Int
day8'2 = maximum . concat . scenic . parseInput

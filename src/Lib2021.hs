module Lib2021
  ( solutions2021
  , groups
  ) where

import           Lib       (Solution (..))

import           Data.List (singleton)

-- Parsing for day1
parseNumList :: String -> [Int]
parseNumList = map read . lines

-- Day 1 solutions
groups :: Int -> [Int] -> [[Int]]
groups size l =
  case size of
    1 -> map singleton l
    _ -> zipWith (:) l (tail (groups (size - 1) l))

day1General :: Int -> String -> Int
day1General n input = do
    let sumGroups = map sum . groups n . parseNumList $ input
    let pairs = zip sumGroups $ drop 1 sumGroups
    length $ filter (uncurry (<)) pairs

day1'1 :: String -> Int
day1'1 = day1General 1

day1'2 :: String -> Int
day1'2 = day1General 3

-- Solution registry
solutions2021 :: [Solution]
solutions2021 =
  [ Solution
      { name = "2021 Day 1.1"
      , testPath = "inputs/2021/tests/day1.txt"
      , dataPath = "inputs/2021/day1.txt"
      , fnc = day1'1
      }
  , Solution
      { name = "2021 Day 1.2"
      , testPath = "inputs/2021/tests/day1.txt"
      , dataPath = "inputs/2021/day1.txt"
      , fnc = day1'2
      }
  ]

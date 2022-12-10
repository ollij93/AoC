module AoC2022.Day01
  ( day1'1
  , day1'2
  ) where

import           Data.List       (sort)
import           Data.List.Split (splitOn)

-- Parsing for day1
parseInventories :: String -> [[Int]]
parseInventories = map (map read . lines) . splitOn "\n\n"

-- Day 1 solutions
day1 :: Int -> String -> Int
day1 nElves = do
  sum . take nElves . reverse . sort . map sum . parseInventories

day1'1 :: String -> Int
day1'1 = day1 1

day1'2 :: String -> Int
day1'2 = day1 3

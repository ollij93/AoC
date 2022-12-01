module Lib
  ( solutions
  ) where

import           Data.List       (sort)
import           Data.List.Split (splitOn)

-- Parsing for day1
parseInventory :: String -> [Int]
parseInventory = map read . lines

parseInventories :: String -> [[Int]]
parseInventories = map parseInventory . splitOn "\n\n"

-- Day 1 solutions
day1General :: Int -> String -> Int
day1General nElves = do
  sum . take nElves . reverse . sort . map sum . parseInventories

day1'1 :: String -> Int
day1'1 = day1General 1

day1'2 :: String -> Int
day1'2 = day1General 3

-- Solution registry
solutions :: [(String, String -> Int)]
solutions = [("Day 1.1", day1'1), ("Day 1.2", day1'2)]

module Lib
  ( solutions
  , isEmpty
  ) where

import           Data.List       (sort)
import           Data.List.Split (splitOn)

isEmpty :: [a] -> Bool
isEmpty x =
  case x of
    [] -> True
    _  -> False

asNum :: String -> Int
asNum s = read s :: Int

splitSegments :: String -> [String]
splitSegments = filter (not . isEmpty) . splitOn "\n\n"

parseInventory :: String -> [Int]
parseInventory = map asNum . lines

parseInventories :: String -> [[Int]]
parseInventories = map parseInventory . splitSegments

takeFirstN :: Int -> [a] -> [a]
takeFirstN n l = [l !! x | x <- [0 .. n - 1]]

sortedSums :: [[Int]] -> [Int]
sortedSums = reverse . sort . map sum

day1General :: Int -> String -> Int
day1General nElves = do
  sum . takeFirstN nElves . sortedSums . parseInventories

solutions :: [(String, String -> Int)]
solutions = [("Day 1.1", day1'1), ("Day 1.2", day1'2)]

day1'1 :: String -> Int
day1'1 = day1General 1

day1'2 :: String -> Int
day1'2 = day1General 3

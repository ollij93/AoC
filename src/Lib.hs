module Lib
  ( Solution(..)
  , process
  , solutions
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
data Solution =
  Solution
    { name     :: String
    , testPath :: String
    , dataPath :: String
    , fnc      :: String -> Int
    }

solutions :: [Solution]
solutions =
  [ Solution
      { name = "Day 1.1"
      , testPath = "inputs/tests/day1.txt"
      , dataPath = "inputs/day1.txt"
      , fnc = day1'1
      }
  , Solution
      { name = "Day 1.2"
      , testPath = "inputs/tests/day1.txt"
      , dataPath = "inputs/day1.txt"
      , fnc = day1'2
      }
  ]

-- Run functions
runSolution :: Solution -> String -> String
runSolution Solution {name = sName, fnc = sFnc} input =
  sName ++ ": " ++ show (sFnc input) ++ "\n"

readAndRun :: (Solution -> FilePath) -> Solution -> IO String
readAndRun pathSelector soln =
  fmap (runSolution soln) $ readFile $ pathSelector soln

process :: (Solution -> FilePath) -> [Solution] -> IO ()
process pathSelector solns = do
  result <- concat <$> mapM (readAndRun pathSelector) solns
  putStrLn result

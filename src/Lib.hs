module Lib
  ( Solution(..)
  , process
  , solutions
  ) where

import Data.Char (ord)
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

-- Parsing for day2
data RPC
  = Rock
  | Paper
  | Scissors

parseRPC :: String -> RPC
parseRPC s =
  case s of
    "A" -> Rock
    "B" -> Paper
    "X" -> Rock
    "Y" -> Paper
    _   -> Scissors

data Result
  = Win
  | Lose
  | Draw

parseResult :: String -> Result
parseResult s =
  case s of
    "X" -> Lose
    "Y" -> Draw
    _   -> Win

parseRound'1 :: String -> (RPC, RPC)
parseRound'1 s = do
  let parts = map parseRPC . words $ s
  (head parts, last parts)

parseRound'2 :: String -> (RPC, RPC)
parseRound'2 s = do
  let opp = parseRPC . head . words $ s
  let res = parseResult . last . words $ s
  let slf =
        (case res of
           Win ->
             (case opp of
                Rock     -> Paper
                Paper    -> Scissors
                Scissors -> Rock)
           Draw -> opp
           Lose ->
             (case opp of
                Rock     -> Scissors
                Paper    -> Rock
                Scissors -> Paper))
  (opp, slf)

-- Day 2 solutions
roundResult :: (RPC, RPC) -> Result
roundResult (a, b) =
  case a of
    Rock ->
      (case b of
         Rock     -> Draw
         Paper    -> Win
         Scissors -> Lose)
    Paper ->
      (case b of
         Rock     -> Lose
         Paper    -> Draw
         Scissors -> Win)
    Scissors ->
      (case b of
         Rock     -> Win
         Paper    -> Lose
         Scissors -> Draw)

roundScore :: (RPC, RPC) -> Int
roundScore (a, b) = do
  let selfScore =
        (case b of
           Rock     -> 1
           Paper    -> 2
           Scissors -> 3)
  selfScore +
    (case roundResult (a, b) of
       Win  -> 6
       Lose -> 0
       Draw -> 3)

day2'1 :: String -> Int
day2'1 = sum . map (roundScore . parseRound'1) . lines

asciiDiff :: Char -> Char -> Int
asciiDiff a b =
  (ord a) - (ord b)

day2'1ascii :: String -> Int
day2'1ascii =
  sum . map (\s -> do
    let opp = (asciiDiff (s !! 0) 'A')
    let slf = (asciiDiff (s !! 2) 'X')
    let rslt = (1 + slf - opp) `mod` 3
    (rslt * 3) + slf + 1
      ) . lines

day2'2 :: String -> Int
day2'2 = sum . map (roundScore . parseRound'2) . lines

day2'2ascii :: String -> Int
day2'2ascii =
  sum . map (\s -> do
    let opp = (asciiDiff (s !! 0) 'A')
    let rslt = (asciiDiff (s !! 2) 'X')
    let slf = (rslt + opp - 1) `mod` 3
    (rslt * 3) + slf + 1
  ) . lines

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
  , Solution
      { name = "Day 2.1"
      , testPath = "inputs/tests/day2.txt"
      , dataPath = "inputs/day2.txt"
      , fnc = day2'1
      }
  , Solution
      { name = "Day 2.2"
      , testPath = "inputs/tests/day2.txt"
      , dataPath = "inputs/day2.txt"
      , fnc = day2'2
      }
  , Solution
      { name = "Day 2.1 (ascii)"
      , testPath = "inputs/tests/day2.txt"
      , dataPath = "inputs/day2.txt"
      , fnc = day2'1ascii
      }
   , Solution
       { name = "Day 2.2 (ascii)"
       , testPath = "inputs/tests/day2.txt"
       , dataPath = "inputs/day2.txt"
       , fnc = day2'2ascii
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

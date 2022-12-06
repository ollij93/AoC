module AoC2022.Day2
  ( day2'1
  , day2'2
  , day2'1ascii
  , day2'2ascii
  ) where

import           Data.Char (ord)

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
asciiDiff a b = ord a - ord b

day2'1ascii :: String -> Int
day2'1ascii =
  sum .
  map
    (\s -> do
       let opp = asciiDiff (head s) 'A'
       let slf = asciiDiff (s !! 2) 'X'
       let rslt = (1 + slf - opp) `mod` 3
       (rslt * 3) + slf + 1) .
  lines

day2'2 :: String -> Int
day2'2 = sum . map (roundScore . parseRound'2) . lines

day2'2ascii :: String -> Int
day2'2ascii =
  sum .
  map
    (\s -> do
       let opp = asciiDiff (head s) 'A'
       let rslt = asciiDiff (s !! 2) 'X'
       let slf = (rslt + opp - 1) `mod` 3
       (rslt * 3) + slf + 1) .
  lines

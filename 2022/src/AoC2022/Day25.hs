module AoC2022.Day25
  ( day25'1
  , day25'2
  ) where

parseChar :: Char -> Int
parseChar c =
  case c of
    '=' -> -2
    '-' -> -1
    '0' -> 0
    '1' -> 1
    _   -> 2

parseLine :: String -> Int
parseLine s =
  case s of
    []   -> 0
    x:xs -> (parseChar x * (5 ^ length xs)) + parseLine xs

parseInput :: String -> [Int]
parseInput = map parseLine . lines

toSNAFU :: Int -> String
toSNAFU n =
  if n == 0
    then ""
    else toSNAFU ((n + 2) `div` 5) ++
         case (n + 2) `mod` 5 of
           0 -> "="
           1 -> "-"
           2 -> "0"
           3 -> "1"
           _ -> "2"

day25'1 :: String -> String
day25'1 = toSNAFU . sum . parseInput

day25'2 :: String -> String
day25'2 = const ""

module AoC2022.Day4
  ( day4'1
  , day4'2
  ) where

import           Data.Bits       (bit, shiftR, (.&.), (.|.))
import           Data.List.Split (splitOn)
import           Data.WideWord   (Int128)

-- Get a tuple of the head and the last of a list
headAndLast :: [a] -> (a, a)
headAndLast l = (head l, last l)

-- Parse functions
parseSections :: String -> Int128
parseSections =
  foldl (.|.) 0 .
  map ((flip shiftR) 1 . bit) .
  (\(x, y) -> [x .. y]) . headAndLast . map read . splitOn "-"

parsePair :: String -> (Int128, Int128)
parsePair s = headAndLast . map parseSections . splitOn "," $ s

-- Processing functions
processPair'1 :: (Int128, Int128) -> Bool
processPair'1 (a, b) = ((a .&. b) == a) || ((a .&. b) == b)

day4'1 :: String -> Int
day4'1 = length . filter processPair'1 . map parsePair . lines

processPair'2 :: (Int128, Int128) -> Bool
processPair'2 = ((/=) 0) . uncurry (.&.)

day4'2 :: String -> Int
day4'2 = length . filter processPair'2 . map parsePair . lines

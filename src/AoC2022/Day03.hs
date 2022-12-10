module AoC2022.Day03
  ( day3'1
  , day3'2
  ) where

import           Data.Bits (bit, countTrailingZeros, (.&.), (.|.))
import           Data.Char (ord)
import           Data.Int  (Int64)
import           Util

-- Split a list into two lists at the halfway point
bisect :: [a] -> ([a], [a])
bisect l = splitAt (length l `div` 2) l

-- Split a list into segments of a given size
segments :: Int -> [a] -> [[a]]
segments n l =
  case l of
    [] -> []
    _  -> take n l : segments n (drop n l)

-- Parse functions
allItems :: Int64
allItems = bit 53 - 1

parseItem :: Char -> Int64
parseItem c =
  bit $
  ord c -
  (if ord c >= ord 'a'
     then ord 'a'
     else ord 'A' - 26)

parsePocket :: [Int64] -> Int64
parsePocket = foldr (.|.) 0

parseBackpack :: [Int64] -> (Int64, Int64)
parseBackpack = mapTuple parsePocket . bisect

-- Processing functions
priority :: Int64 -> Int
priority = (+) 1 . countTrailingZeros

processBackpack :: (Int64, Int64) -> Int
processBackpack (a, b) = priority $ a .&. b

day3'1 :: String -> Int
day3'1 = sum . map (processBackpack . parseBackpack . map parseItem) . lines

processGroupBackpacks :: [Int64] -> Int
processGroupBackpacks = priority . foldr (.&.) allItems

day3'2 :: String -> Int
day3'2 =
  sum .
  map processGroupBackpacks .
  segments 3 . map (parsePocket . map parseItem) . lines

module Lib
    ( day1
    ) where

import Data.List.Split
import Data.List

isEmpty :: [a] -> Bool
isEmpty x =
  case x of
    [] -> True
    _ -> False

asNum :: String -> Int
asNum s =
    read s :: Int

for :: [a] -> (a->b) -> [b]
for = flip map

day1 :: String -> Int
day1 input = do
    let invStrings = filter (not . isEmpty) $ splitOn "\n\n" input
    let inventories = for invStrings $ map asNum . lines
    let invTotals = map sum inventories
    let rankedTotals = (reverse . sort) invTotals
    sum [rankedTotals !! x | x <- [0..2]]

module AoC2022.Day20
  ( day20'1
  , day20'2
  ) where

import           Debug.Trace (trace)
import           Util        (dbg)

insertAt :: Int -> a -> [a] -> [a]
insertAt idx item l = pre ++ [item] ++ post
  where
    (pre, post) = splitAt idx l

mixOne :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
mixOne curr (origIdx, value) = do
  let (pre, post') = break (== (origIdx, value)) curr
  let post = drop 1 post'
  let currIdx = length pre
  let newIdx = (value + currIdx) `mod` (length curr - 1)
  if newIdx == currIdx
    then curr
    else insertAt newIdx (origIdx, value) (pre ++ post)

mix :: Int -> Int -> [Int] -> [Int]
mix numMixes decryptionKey orig =
  map snd $
  iterate
    (\currentIndexed -> foldl mixOne currentIndexed origIndexed)
    origIndexed !!
  numMixes
  where
    origIndexed = zipWith (\i n -> (i, n * decryptionKey)) [0 ..] orig

sample :: [Int] -> Int
sample l' = (l !! a) + (l !! b) + (l !! c)
  where
    l = (\(x, y) -> y ++ x) $ break (== 0) l'
    a = 1000 `mod` length l
    b = 2000 `mod` length l
    c = 3000 `mod` length l

day20'1 :: String -> Int
day20'1 = sample . mix 1 1 . map read . lines

day20'2 :: String -> Int
day20'2 = sample . mix 10 811589153 . map read . lines

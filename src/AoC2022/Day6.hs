module AoC2022.Day6
  ( day6'1
  , day6'2
  ) where

allUniq :: Eq a => [a] -> Bool
allUniq = fst . foldr (\v (b, l) -> (b && notElem v l, v : l)) (True, [])

markerPos :: Int -> Int -> String -> Int
markerPos markerLen startidx s =
  if allUniq $ take markerLen s
    then startidx + markerLen
    else markerPos markerLen (startidx + 1) (drop 1 s)

day6 :: Int -> String -> String
day6 markerLen = drop 1 . concatMap ((:) '\n' . show . markerPos markerLen 0) . lines

day6'1 :: String -> String
day6'1 = day6 4

day6'2 :: String -> String
day6'2 = day6 14

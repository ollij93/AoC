module Util
  ( mapTuple
  , mapTuple3
  , splitOnce
  , every
  ) where

-- Map members of a tuple
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)

mapTuple3 :: (a -> b) -> (a,a,a) -> (b,b,b)
mapTuple3 f (x, y, z) = (f x, f y, f z)

-- Split a string once into two substring
splitOnce :: Char -> String -> (String, String)
splitOnce c = fmap (drop 1) . break (c ==)

-- Take every nth element of a list
every :: Int -> [a] -> [a]
every n l =
  case drop (n - 1) l of
    x:l' -> x : every n l'
    []   -> []

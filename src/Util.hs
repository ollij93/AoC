module Util
  ( mapTuple
  , splitOnce
  ) where

-- Map members of a tuple
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)

splitOnce :: Char -> String -> (String, String)
splitOnce c = fmap (drop 1) . break (c ==)

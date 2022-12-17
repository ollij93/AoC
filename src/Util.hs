module Util
  ( dbg
  , every
  , mapTuple
  , mapTuple3
  , segments
  , splitOnce
  , spacepad
  ) where

import           Debug.Trace (trace)

-- Map members of a tuple
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)

mapTuple3 :: (a -> b) -> (a, a, a) -> (b, b, b)
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

-- Debug a showable
dbg :: Show a => String -> a -> a
dbg s t = trace (s ++ show t ++ "  ") t

-- Split a list into segments of a given size
segments :: Int -> [a] -> [[a]]
segments n l =
  case l of
    [] -> []
    _  -> take n l : segments n (drop n l)

-- Pad a string with spaces to make it at least the given length
spacepad :: Int -> String -> String
spacepad n s = replicate (n - length s) ' ' ++ s

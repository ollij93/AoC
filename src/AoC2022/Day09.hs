module AoC2022.Day09
  ( day9'1
  , day9'2
  ) where

import           Data.List   (nub)

type Vec = (Int, Int)

data Dir
  = Up
  | Dwn
  | Lft
  | Rght

type Move = (Dir, Int)

dirVec :: Dir -> Vec
dirVec dir =
  case dir of
    Up   -> (0, 1)
    Dwn  -> (0, -1)
    Lft  -> (-1, 0)
    Rght -> (1, 0)

parseDir :: Char -> Dir
parseDir c =
  case c of
    'U' -> Up
    'D' -> Dwn
    'L' -> Lft
    _   -> Rght

parseMove :: String -> Move
parseMove s = (parseDir $ head s, read $ drop 2 s)

parseInput :: String -> [Move]
parseInput = map parseMove . lines

moveOnce :: Dir -> [Vec] -> [Vec]
moveOnce dir trail = do
  let (posx, posy) = head trail
  let (dx, dy) = dirVec dir
  let newpos = (posx + dx, posy + dy)
  newpos : trail

-- Creates a "trail" where the current position is on top
moveToTrail :: Move -> [Vec] -> [Vec]
moveToTrail (dir, dist) trail =
  case dist of
    0 -> trail
    _ -> moveToTrail (dir, dist - 1) . moveOnce dir $ trail

-- Converts move instructions to a trail starting at the origin
movesToTrail :: [Move] -> [Vec]
movesToTrail = reverse . foldl (flip moveToTrail) [(0, 0)]

-- Determine the new position when following a target that's just taken a step
followTgt :: Vec -> Vec -> Vec
followTgt (tgtx, tgty) (curx, cury) = do
  let xtwo = tgtx - curx > 1 || tgtx - curx < -1
  let ytwo = tgty - cury > 1 || tgty - cury < -1
  if ytwo || xtwo -- Two steps away in either direction so need to move
    then ( curx +
           (if tgtx > curx
              then 1
              else if tgtx < curx
                     then -1
                     else 0)
         , cury +
           (if tgty > cury
              then 1
              else if tgty < cury
                     then -1
                     else 0))
    else (curx, cury)

-- Creates a new trail by following an existing trail
follow :: [Vec] -> [Vec]
follow =
  reverse .
  foldl (\newtrail tgt -> followTgt tgt (head newtrail) : newtrail) [(0, 0)] .
  tail -- start is always 0,0 which we can drop

day9'1 :: String -> Int
day9'1
 = length . nub . follow . movesToTrail . parseInput

nFollow :: Int -> [Vec] -> [Vec]
nFollow i =
  case i of
    0 -> id
    _ -> nFollow (i - 1) . follow

day9'2 :: String -> Int
day9'2 = length . nub . nFollow 9 . movesToTrail . parseInput

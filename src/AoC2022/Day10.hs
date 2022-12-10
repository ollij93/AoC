module AoC2022.Day10
  ( day10'1
  , day10'2
  ) where

import           Util (dbg, segments)

data Instruction
  = NoOp
  | AddX Int

parseInput :: String -> [Instruction]
parseInput =
  map
    (\l ->
       case head l of
         'a' -> AddX . read $ drop 5 l
         _   -> NoOp) .
  lines

processInstruction :: Instruction -> Int -> [Int]
processInstruction inst reg =
  case inst of
    NoOp   -> [reg]
    AddX n -> [reg + n, reg]

processInstructions :: [Instruction] -> [Int]
processInstructions =
  reverse .
  foldl (\reglst inst -> processInstruction inst (head reglst) ++ reglst) [1]

sumInteresting :: [Int] -> Int
sumInteresting l =
  foldl
    (\total n -> ((l !! (n - 1)) * n) + total)
    0
    [20, 60, 100, 140, 180, 220]

day10'1 :: String -> Int
day10'1 = sumInteresting . processInstructions . parseInput

crtVal :: Int -> Int -> Char
crtVal idx reg =
  if idx < reg - 1 || idx > reg + 1
    then '.'
    else '#'

day10'2 :: String -> String
day10'2 =
  concatMap (\a -> '\n' : zipWith crtVal [0 ..] a) .
  segments 40 . take 240 . processInstructions . parseInput

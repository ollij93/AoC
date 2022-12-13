module AoC2022.Day13
  ( day13'1
  , day13'2
  ) where

import           Data.List       (elemIndex, sort)
import           Data.List.Split (splitOn)
import           Data.Maybe      (catMaybes)

data PacketData
  = List [PacketData]
  | Num Int

instance Eq PacketData where
  Num a == Num b   = a == b
  Num a == List b  = List [Num a] == List b
  List a == Num b  = List a == List [Num b]
  List a == List b = a == b

instance Ord PacketData where
  Num a <= Num b   = a <= b
  Num a <= List b  = List [Num a] <= List b
  List a <= Num b  = List a <= List [Num b]
  List a <= List b = a <= b

instance Show PacketData where
  show pdata =
    case pdata of
      List content ->
        "[" ++
        (\s -> take (length s - 1) s) (concatMap (\x -> show x ++ ",") content) ++
        "]"
      Num n -> show n

parsePacketList :: [[PacketData]] -> String -> [[PacketData]]
parsePacketList lstack s = do
  case s of
    [] -> lstack
    x:xs ->
      case x
        -- Starting a new list, push an empty list onto the stack and continue parsing
            of
        '[' -> parsePacketList ([] : lstack) xs
        ']'
            -- If finishing a list, remove the top of the stack and append it to the
            -- end of the next list of the stack
         -> do
          let donel = head lstack
          let next = head . tail $ lstack
          let newtop = next ++ [List donel]
          let remainder = drop 2 lstack
          let newstack = newtop : remainder
          parsePacketList newstack xs
        ',' -> parsePacketList lstack xs
        _
            -- Must be a number, find the end of it (either a , or a ]) and read it
         -> do
          let idx = minimum $ catMaybes [elemIndex ',' s, elemIndex ']' s]
          let num = read $ take idx s
          let rest = drop idx s
          let newstack = (head lstack ++ [Num num]) : tail lstack
          parsePacketList newstack rest

parseLine :: String -> PacketData
parseLine = head . head . parsePacketList [[]]

day13'1 :: String -> Int
day13'1 =
  sum .
  zipWith
    (\idx pktlst ->
       if pktlst == sort pktlst
         then idx
         else 0)
    [1 ..] .
  map (map parseLine . lines) . splitOn "\n\n"

divPacket1 :: PacketData
divPacket1 = List [List [Num 2]]

divPacket2 :: PacketData
divPacket2 = List [List [Num 6]]

day13'2 :: String -> Int
day13'2 =
  product .
  map (+ 1) .
  catMaybes .
  (\l -> [elemIndex divPacket1 l, elemIndex divPacket2 l]) .
  sort .
  ([divPacket1, divPacket2] ++) . map parseLine . filter (not . null) . lines

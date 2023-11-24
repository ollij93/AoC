module AoC2022.Day22
  ( day22'1
  , day22'2
  ) where

import           Data.List       (findIndex, transpose)
import           Data.List.Split (splitOn)
import           Data.Maybe      (fromMaybe)

data Block
  = Void
  | Air
  | Wall
  deriving (Show, Eq)

type Board = [[Block]]

parseBlock :: Char -> Block
parseBlock c =
  case c of
    ' ' -> Void
    '.' -> Air
    _   -> Wall

parseBoardRow :: String -> [Block]
parseBoardRow = map parseBlock

parseBoard :: String -> Board
parseBoard = map parseBoardRow . lines

data LorR
  = L
  | R
  deriving (Show, Eq)

data Instruction
  = Walk Int
  | Turn LorR
  deriving (Show, Eq)

parseInstructions :: String -> [Instruction]
parseInstructions s =
  case s of
    [] -> []
    c:l ->
      case c of
        'L' -> Turn L : parseInstructions l
        'R' -> Turn R : parseInstructions l
        _ -> Walk (read nS) : parseInstructions (drop (length nS) s)
          where nS = takeWhile (\x -> x /= 'L' && x /= 'R') s

data CardinalDir
  = N
  | E
  | S
  | W
  deriving (Show, Eq)

data Model =
  Model
    { board        :: Board
    , instructions :: [Instruction]
    , facing       :: CardinalDir
    , position     :: (Int, Int)
    }
  deriving (Show, Eq)

parseInput :: String -> Model
parseInput s =
  Model
    { board = brd
    , instructions = parseInstructions instS
    , facing = E
    , position = (length . takeWhile (== Void) $ head brd, 0)
    }
  where
    parts = splitOn "\n\n" s
    brdS = head parts
    instS = last parts
    brd = parseBoard brdS

password :: Model -> Int
password model = 1000 * row + 4 * col + fac
  where
    row = 1 + snd (position model)
    col = 1 + fst (position model)
    fac =
      case facing model of
        E -> 0
        S -> 1
        W -> 2
        N -> 3

updateWalk :: Int -> Model -> Model
updateWalk n model =
  case facing model of
    N -> model {position = (fst (position model), moddedIdx)}
      where col = transpose (board model) !! fst (position model)
            ahead =
              reverse . (\(a, b) -> b ++ a) . splitAt (snd (position model)) $
              col
            loopedAhead = cycle . filter (/= Void) $ ahead
            -- Will take this many steps before stopping
            stopsAfter = fromMaybe n (findIndex (/= Air) $ take n loopedAhead)
            -- Would this take us through a void?
            resultingIdx =
              snd (position model) - stopsAfter -
              if Void `elem` take stopsAfter ahead
                then length . filter (== Void) $ ahead
                else 0
            moddedIdx = resultingIdx `mod` length col
    S -> model {position = (fst (position model), moddedIdx)}
      where col = transpose (board model) !! fst (position model)
            ahead =
              (\(a, b) -> b ++ a) . splitAt (snd (position model) + 1) $ col
            loopedAhead = cycle . filter (/= Void) $ ahead
            -- Will take this many steps before stopping
            stopsAfter = fromMaybe n (findIndex (/= Air) $ take n loopedAhead)
            -- Would this take us through a void?
            resultingIdx =
              snd (position model) + stopsAfter +
              if Void `elem` take stopsAfter ahead
                then length . filter (== Void) $ ahead
                else 0
            moddedIdx = resultingIdx `mod` length col
    E -> model {position = (moddedIdx, snd (position model))}
      where row = board model !! snd (position model)
            ahead =
              (\(a, b) -> b ++ a) . splitAt (fst (position model) + 1) $ row
            loopedAhead = cycle . filter (/= Void) $ ahead
            -- Will take this many steps before stopping
            stopsAfter = fromMaybe n (findIndex (/= Air) $ take n loopedAhead)
            -- Would this take us through a void?
            resultingIdx =
              fst (position model) + stopsAfter +
              if Void `elem` take stopsAfter ahead
                then length . filter (== Void) $ ahead
                else 0
            moddedIdx = resultingIdx `mod` length row
    W -> model {position = (moddedIdx, snd (position model))}
      where row = board model !! snd (position model)
            ahead =
              reverse . (\(a, b) -> b ++ a) . splitAt (fst (position model)) $
              row
            loopedAhead = cycle . filter (/= Void) $ ahead
            -- Will take this many steps before stopping
            stopsAfter = fromMaybe n (findIndex (/= Air) $ take n loopedAhead)
            -- Would this take us through a void?
            resultingIdx =
              fst (position model) - stopsAfter -
              if Void `elem` take stopsAfter ahead
                then length . filter (== Void) $ ahead
                else 0
            moddedIdx = resultingIdx `mod` length row

update :: Model -> Instruction -> Model
update model inst =
  case inst of
    Turn dir ->
      model
        { facing =
            case dir of
              L ->
                case facing model of
                  N -> W
                  E -> N
                  S -> E
                  W -> S
              R ->
                case facing model of
                  N -> E
                  E -> S
                  S -> W
                  W -> N
        }
    Walk n -> updateWalk n model

run :: Model -> Model
run model = foldl update model (instructions model)

day22'1 :: String -> Int
day22'1 = password . run . parseInput

day22'2 :: String -> Int
day22'2 = length

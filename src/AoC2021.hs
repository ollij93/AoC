module AoC2021
  ( solutions
  ) where

import           AoC             (Solution (..))

import           Data.List       (singleton)
import           Data.List.Split (splitOn)

-- Parsing for day1
parseNumList :: String -> [Int]
parseNumList = map read . lines

-- Day 1 solutions
groups :: Int -> [Int] -> [[Int]]
groups size l =
  case size of
    1 -> map singleton l
    _ -> zipWith (:) l (tail (groups (size - 1) l))

day1General :: Int -> String -> Int
day1General n input = do
  let sumGroups = map sum . groups n . parseNumList $ input
  let pairs = zip sumGroups $ drop 1 sumGroups
  length $ filter (uncurry (<)) pairs

day1'1 :: String -> Int
day1'1 = day1General 1

day1'2 :: String -> Int
day1'2 = day1General 3

-- Day 2 solutions
data Direction
  = Forward
  | Up
  | Down

parseDirection :: String -> Direction
parseDirection s =
  case s of
    "up"   -> Up
    "down" -> Down
    _      -> Forward

parseInstruction :: String -> (Direction, Int)
parseInstruction s = do
  let dirPart = takeWhile (/= ' ') s
  let velPart = dropWhile (/= ' ') s
  (parseDirection dirPart, read velPart)

parseInstructions :: String -> [(Direction, Int)]
parseInstructions = map parseInstruction . lines

type ProcessInstructionFnc
   = (Int, Int, Int) -> (Direction, Int) -> (Int, Int, Int)

day2General :: ProcessInstructionFnc -> String -> Int
day2General processInstruction =
  (\(x, y, _) -> x * y) . foldl processInstruction (0, 0, 0) . parseInstructions

processInstruction'1 :: ProcessInstructionFnc
processInstruction'1 (x, y, aim) (dir, spd) =
  case dir of
    Forward -> (x + spd, y, aim)
    Up      -> (x, y - spd, aim)
    Down    -> (x, y + spd, aim)

day2'1 :: String -> Int
day2'1 = day2General processInstruction'1

processInstruction'2 :: ProcessInstructionFnc
processInstruction'2 (x, y, aim) (dir, spd) =
  case dir of
    Forward -> (x + spd, y + aim * spd, aim)
    Up      -> (x, y, aim - spd)
    Down    -> (x, y, aim + spd)

day2'2 :: String -> Int
day2'2 = day2General processInstruction'2

-- Day 24
data ALUReg
  = W
  | X
  | Y
  | Z

data ALURegOrInt
  = Reg ALUReg
  | N Int

index :: (Int, Int, Int, Int) -> ALUReg -> Int
index (w, x, y, z) r =
  case r of
    W -> w
    X -> x
    Y -> y
    Z -> z

value :: (Int, Int, Int, Int) -> ALURegOrInt -> Int
value reg r =
  case r of
    Reg r' -> index reg r'
    N v    -> v

set :: ALUReg -> Int -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
set r v (w, x, y, z) =
  case r of
    W -> (v, x, y, z)
    X -> (w, v, y, z)
    Y -> (w, x, v, z)
    Z -> (w, x, y, v)

parseALUReg :: String -> Maybe ALUReg
parseALUReg c =
  case c of
    "w" -> Just W
    "x" -> Just X
    "y" -> Just Y
    "z" -> Just Z
    _   -> Nothing

parseALURegOrInt :: String -> ALURegOrInt
parseALURegOrInt s =
  case parseALUReg s of
    Nothing -> N $ read s
    Just x  -> Reg x

data ALUInstruction
  = Inp ALUReg
  | Add ALUReg ALURegOrInt
  | Mul ALUReg ALURegOrInt
  | Div ALUReg ALURegOrInt
  | Mod ALUReg ALURegOrInt
  | Eql ALUReg ALURegOrInt

parseALUInstruction :: String -> Maybe ALUInstruction
parseALUInstruction s = do
  let inst = head $ splitOn " " s
  let a = parseALUReg . head . tail $ splitOn " " s
  let b = parseALURegOrInt . last $ splitOn " " s
  fmap
    (\a' ->
       case inst of
         "inp" -> Inp (a')
         "add" -> Add (a') (b)
         "mul" -> Mul (a') (b)
         "div" -> Div (a') (b)
         "mod" -> Mod (a') (b)
         _     -> Eql (a') (b))
    a

parseALUInstructions :: String -> [ALUInstruction]
parseALUInstructions =
  (\l -> [x | Just x <- l]) . map parseALUInstruction . lines

processALUInstruction ::
     ([Int], (Int, Int, Int, Int))
  -> ALUInstruction
  -> ([Int], (Int, Int, Int, Int))
processALUInstruction (inp, reg) inst =
  case inst of
    Inp a -> (tail inp, set a (head inp) reg)
    Add a b -> (inp, set a ((reg `index` a) + (reg `value` b)) reg)
    Mul a b -> (inp, set a ((reg `index` a) * (reg `value` b)) reg)
    Div a b -> (inp, set a ((reg `index` a) `div` (reg `value` b)) reg)
    Mod a b -> (inp, set a ((reg `index` a) `mod` (reg `value` b)) reg)
    Eql a b ->
      ( inp
      , set
          a
          (if (reg `index` a) == (reg `value` b)
             then 1
             else 0)
          reg)

day24'1 :: String -> Int
day24'1 =
  (\reg -> reg `index` Z) .
  snd .
  foldl processALUInstruction ([9 | _ <- [1 .. 14]], (0, 0, 0, 0)) .
  parseALUInstructions

-- Solution registry
solutions :: [Solution]
solutions =
  [ Solution
      { name = "2021-Day1.1"
      , testPath = "inputs/2021/tests/day1.txt"
      , dataPath = "inputs/2021/day1.txt"
      , fnc = day1'1
      }
  , Solution
      { name = "2021-Day1.2"
      , testPath = "inputs/2021/tests/day1.txt"
      , dataPath = "inputs/2021/day1.txt"
      , fnc = day1'2
      }
  , Solution
      { name = "2021-Day2.1"
      , testPath = "inputs/2021/tests/day2.txt"
      , dataPath = "inputs/2021/day2.txt"
      , fnc = day2'1
      }
  , Solution
      { name = "2021-Day2.2"
      , testPath = "inputs/2021/tests/day2.txt"
      , dataPath = "inputs/2021/day2.txt"
      , fnc = day2'2
      }
  , Solution
      { name = "2021 Day 24.1"
      , testPath = "inputs/2021/tests/day24_negate.txt"
      , dataPath = "inputs/2021/day24.txt"
      , fnc = day24'1
      }
  ]

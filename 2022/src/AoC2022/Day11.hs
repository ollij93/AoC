module AoC2022.Day11
  ( day11'1
  , day11'2
  ) where

import           Data.List       (sort)
import           Data.List.Split (splitOn)

data MonkeyOp
  = Add Int
  | Mult Int
  | Sqr

data Monkey =
  Monkey
    { items        :: [Int]
    , operation    :: MonkeyOp
    , test         :: Int
    , trueTgt      :: Int
    , falseTgt     :: Int
    , numInspected :: Int
    }

parseItems :: String -> [Int]
parseItems = map read . splitOn ", " . drop (length "  Starting items: ")

parseOp :: String -> MonkeyOp
parseOp s =
  case s !! length "  Operation: new = old " of
    '*' ->
      case s of
        "  Operation: new = old * old" -> Sqr
        _ -> Mult (read . drop (length "  Operation: new = old * ") $ s)
    _ -> Add (read . drop (length "  Operation: new = old + ") $ s)

parseTest :: String -> Int
parseTest = read . drop (length "  Test: divisible by ")

parseMonkey :: String -> Monkey
parseMonkey s = do
  let lns = lines s
  let itms = parseItems (lns !! 1)
  let op = parseOp (lns !! 2)
  let tst = parseTest (lns !! 3)
  let truetgt =
        read . drop (length "    If true: throw to monkey ") $ (lns !! 4)
  let falsetgt =
        read . drop (length "    If false: throw to monkey ") $ (lns !! 5)
  Monkey
    { items = itms
    , operation = op
    , test = tst
    , trueTgt = truetgt
    , falseTgt = falsetgt
    , numInspected = 0
    }

takeTurn :: (Int -> Int) -> Int -> [Monkey] -> [Monkey]
takeTurn worryLimiter monkeyId monkeys = do
  let monkey = monkeys !! monkeyId
  case items monkey of
    [] -> monkeys
    x:_ -> do
      let worry =
            worryLimiter $
            case operation monkey of
              Add n  -> x + n
              Mult n -> x * n
              Sqr    -> x * x
      let tgt =
            if (worry `mod` test monkey) == 0
              then trueTgt monkey
              else falseTgt monkey
      takeTurn worryLimiter monkeyId .
        zipWith
          (\_id m ->
             m
               { items =
                   (if _id == monkeyId
                      then tail
                      else id)
                     (items m) ++
                   [worry | _id == tgt]
               , numInspected =
                   if _id == monkeyId
                     then numInspected m + 1
                     else numInspected m
               })
          [0 ..] $
        monkeys

takeFullTurn :: (Int -> Int) -> [Monkey] -> [Monkey]
takeFullTurn worryLimiter ms =
  foldl (flip $ takeTurn worryLimiter) ms [0 .. (length ms - 1)]

parseInput :: String -> [Monkey]
parseInput = map parseMonkey . splitOn "\n\n"

run :: (Int -> Int) -> Int -> [Monkey] -> [Monkey]
run worryLimiter numRounds monkeys =
  foldr (const $ takeFullTurn worryLimiter) monkeys [1 .. numRounds]

getScore :: [Monkey] -> Int
getScore = product . take 2 . reverse . sort . map numInspected

day11'1 :: String -> Int
day11'1 = getScore . run (`div` 3) 20 . parseInput

day11'2 :: String -> Int
day11'2 s = do
  let monkeys = parseInput s
  let base = product . map test $ monkeys
  getScore . run (`mod` base) 10000 $ monkeys

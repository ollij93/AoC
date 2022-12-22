module AoC2022.Day21
  ( day21'1
  , day21'2
  ) where

import           Data.HashMap (Map, (!))
import qualified Data.HashMap as Map
import           Debug.Trace  (trace)
import           Util         (dbg)

type MKey = String

data MOp
  = Add
  | Sub
  | Mul
  | Div
  deriving (Show, Eq)

data Monkey
  = Const Int
  | Operation MOp MKey MKey
  deriving (Show, Eq)

parseOp :: String -> MOp
parseOp s =
  case s of
    "+" -> Add
    "-" -> Sub
    "*" -> Mul
    _   -> Div

parseMonkey :: String -> (MKey, Monkey)
parseMonkey s =
  case length wrds of
    2 -> (mkey, Const . read $ wrds !! 1)
    _ -> (mkey, Operation (parseOp (wrds !! 2)) (wrds !! 1) (wrds !! 3))
  where
    wrds = words s
    mkey = init (head wrds)

parseInput :: String -> Map MKey Monkey
parseInput = Map.fromList . map parseMonkey . lines

calculate :: MKey -> Map MKey Monkey -> Double
calculate mkey rules =
  case rules ! mkey of
    Const n -> fromIntegral n
    Operation op a b ->
      case op of
        Add -> a' + b'
        Sub -> a' - b'
        Mul -> a' * b'
        Div -> a' / b'
      where a' = calculate a rules
            b' = calculate b rules

day21'1 :: String -> Int
day21'1 = floor . calculate "root" . parseInput

mulPowerSeries :: Map Int Double -> Map Int Double -> Map Int Double
mulPowerSeries a b =
  foldl (\m (k, v) -> Map.insertWith (+) k v m) Map.empty $
  foldl (\l (p, v) -> l ++ map (\(p', v') -> (p + p', v * v')) a') [] b'
  where
    a' = Map.toList a
    b' = Map.toList b

-- This isn't actually valid I now realise, but looking throught the debug output shows that b is only ever a constant, so this does work correctly
divPowerSeries :: Map Int Double -> Map Int Double -> Map Int Double
divPowerSeries a b =
  foldl (\m (k, v) -> Map.insertWith (+) k v m) Map.empty $
  foldl (\l (p, v) -> l ++ map (\(p', v') -> (p' - p, v' / v)) a') [] b'
  where
    a' = Map.toList a
    b' = Map.toList b

impact :: MKey -> Map MKey Monkey -> Map Int Double
impact mkey rules =
  if mkey == "humn"
    then Map.insert 1 1 Map.empty -- humn has an impact of y = 1x+0 on humn
    else case rules ! mkey of
           Const n
            -- impact of humn on a constant is y = 0x + n
            -> Map.insert 0 (fromIntegral n) Map.empty
           Operation op a b ->
             case op of
               Add -> Map.unionWith (+) a' b'
               Sub -> Map.unionWith (+) a' (Map.map (* (-1)) b')
               Mul -> mulPowerSeries a' b'
               Div -> divPowerSeries a' b'
             where a' = impact a rules
                   b' = impact b rules

subOutRoot :: Map MKey Monkey -> Map MKey Monkey
subOutRoot rules = Map.insert "root" (Operation Sub a b) rules
  where
    (a, b) =
      case rules ! "root" of
        Operation _ a' b' -> (a', b')
        _                 -> ("a", "b")

subOutHumn :: Int -> Map MKey Monkey -> Map MKey Monkey
subOutHumn val = Map.insert "humn" (Const val)

day21'2 :: String -> Int
day21'2 s = do
  let rules = subOutRoot . parseInput $ s
  let imp = (\t -> trace ("IMPACT " ++ show t) t) $ impact "root" rules
  -- From inspection the resulting power series is linear in humn, so can just do straight map
  let res =
        dbg "RES" $
        floor (Map.findWithDefault 0 0 imp / Map.findWithDefault 0 1 imp) * (-1)
  let actualRoot = floor $ dbg "ROOT" $ calculate "root" $ subOutHumn res rules
  if actualRoot == 0
    then res
      -- Try nearby values if the exact calculation fails
    else do
      let valsToTry = map (+ res) [-10 .. 10]
      let (res, _) =
            foldl
              (\(bstn, bstv) (n, v) ->
                 if abs v < abs bstv
                   then (n, v)
                   else (bstn, bstv))
              (-1, 999999) $
            map
              (\n -> dbg "Tryed" (n, calculate "root" $ subOutHumn n rules))
              valsToTry
      res

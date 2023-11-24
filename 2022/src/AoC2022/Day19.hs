module AoC2022.Day19
  ( day19'1
  , day19'2
  ) where

import           Data.List   (sort)
import           Data.Set    (Set)
import qualified Data.Set    as Set

data Price =
  Price
    { _ore      :: Int
    , _clay     :: Int
    , _obsidian :: Int
    }
  deriving (Show)

data Blueprint =
  Blueprint
    { orePrice      :: Price
    , clayPrice     :: Price
    , obsidianPrice :: Price
    , geodePrice    :: Price
    }
  deriving (Show)

parseLine :: String -> Blueprint
parseLine s =
  Blueprint
    { orePrice = Price {_ore = read (wrds !! 6), _clay = 0, _obsidian = 0}
    , clayPrice = Price {_ore = read (wrds !! 12), _clay = 0, _obsidian = 0}
    , obsidianPrice =
        Price
          {_ore = read (wrds !! 18), _clay = read (wrds !! 21), _obsidian = 0}
    , geodePrice =
        Price
          {_ore = read (wrds !! 27), _clay = 0, _obsidian = read (wrds !! 30)}
    }
  where
    wrds = words s

data Inventory =
  Inventory
    { ore          :: Int
    , clay         :: Int
    , obsidian     :: Int
    , geodes       :: Int
    , oreBots      :: Int
    , clayBots     :: Int
    , obsidianBots :: Int
    , geodeBots    :: Int
    }
  deriving (Show, Eq)

instance Ord Inventory where
  a <= b = asTuple a <= asTuple b

-- Convert an inventory to a tuple to be sorted (most significant params come
-- first in the tuple to achieve this)
asTuple :: Inventory -> (Int, Int, Int, Int, Int, Int, Int, Int)
asTuple inv =
  ( geodeBots inv
  , geodes inv
  , obsidianBots inv
  , obsidian inv
  , clayBots inv
  , clay inv
  , oreBots inv
  , ore inv)

initialInventory :: Inventory
initialInventory =
  Inventory
    { ore = 0
    , clay = 0
    , obsidian = 0
    , geodes = 0
    , oreBots = 1
    , clayBots = 0
    , obsidianBots = 0
    , geodeBots = 0
    }

canAfford :: Price -> Inventory -> Bool
canAfford price inv =
  (ore inv >= _ore price) &&
  (clay inv >= _clay price) && (obsidian inv >= _obsidian price)

spend :: Price -> Inventory -> Inventory
spend price inv =
  inv
    { ore = ore inv - _ore price
    , clay = clay inv - _clay price
    , obsidian = obsidian inv - _obsidian price
    }

-- Determine all possibly inventories resulting from making any bot that can be
-- afforded
makeBot :: Blueprint -> Inventory -> Set Inventory
makeBot bp inv = buyGeode . buyObsidian . buyClay . buyOre $ Set.singleton inv
  where
    gp = geodePrice bp
    buyGeode =
      if canAfford gp inv
        then Set.insert $ (spend gp inv) {geodeBots = geodeBots inv + 1}
        else id
    obp = obsidianPrice bp
    buyObsidian =
      if canAfford obp inv
        then Set.insert $ (spend obp inv) {obsidianBots = obsidianBots inv + 1}
        else id
    cp = clayPrice bp
    buyClay =
      if canAfford cp inv
        then Set.insert $ (spend cp inv) {clayBots = clayBots inv + 1}
        else id
    orp = orePrice bp
    buyOre =
      if canAfford orp inv
        then Set.insert $ (spend orp inv) {oreBots = oreBots inv + 1}
        else id

tick :: Blueprint -> Inventory -> Set Inventory
tick bp inv =
  Set.map
    (\x ->
       x
         { ore = ore x + oreBots inv
         , clay = clay x + clayBots inv
         , obsidian = obsidian x + obsidianBots inv
         , geodes = geodes x + geodeBots inv
         }) $
  makeBot bp inv

-- Filter out any inventories which have less then, or equal values for all
-- items than another inventory in the set
filterInvs :: Set Inventory -> Set Inventory
filterInvs invs =
  Set.filter
    (\inv ->
       not $
       any
         (\other ->
            inv /= other &&
            clay inv <= clay other &&
            ore inv <= ore other &&
            obsidian inv <= obsidian other &&
            geodes inv <= geodes other &&
            oreBots inv <= oreBots other &&
            clayBots inv <= clayBots other &&
            obsidianBots inv <= obsidianBots other &&
            geodeBots inv <= geodeBots other)
         invs')
    invs
  where
    invs' = Set.toList invs

keepBest :: Int -> Set Inventory -> Set Inventory
keepBest n = Set.fromList . take n . reverse . sort . Set.toList

run :: Int -> Blueprint -> Int
run minutes bp
  -- Sim N-1 minutes and on the last minute just make as many geodes as
  -- possible, ignoring other values
 =
  maximum . Set.map (\inv -> geodes inv + geodeBots inv) $
  iterate
    (keepBest 100 . filterInvs . Set.unions . Set.map (tick bp))
    (Set.singleton initialInventory) !!
  (minutes - 1)

qualityLevel :: Int -> Blueprint -> Int
qualityLevel bpid bp = bpid * run 24 bp

day19'1 :: String -> Int
day19'1 = sum . zipWith qualityLevel [1 ..] . map parseLine . lines

day19'2 :: String -> Int
day19'2 =
  product .
  map (run 32 . parseLine) . take 3 . lines

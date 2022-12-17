module AoC2022.Day16
  ( day16'1
  , day16'2
  ) where

import           Data.Function ((&))
import           Data.HashMap  (fromList, toList, unionWith, (!))
import qualified Data.HashMap  as Map
import           Data.List     (delete, sort)

data Node =
  Node
    { value :: Int
    , links :: Map.Map String Int
    }

instance Show Node where
  show n = "Node(value=" ++ show (value n) ++ ",links=" ++ show (links n) ++ ")"

type Graph = Map.Map String Node

data ActorState
  = MovingTo String Int
  | OpeningValve String

data Model =
  Model
    { graph            :: Graph
    , valvesVisited    :: [String]
    , valvesOpened     :: [String]
    , selfState        :: ActorState
    , elephantState    :: ActorState
    , preasureRelieved :: Int
    , timeRemaining    :: Int
    }

rate :: Model -> Int
rate m = sum . map (\n -> value (graph m ! n)) . valvesOpened $ m

updateSelf :: Model -> [Model]
updateSelf model =
  case selfState model of
    MovingTo tgt remaining ->
      if remaining <= 1
        -- If we're only one step from the target start opening the valve in the next minute
        then [model {selfState = OpeningValve tgt}]
        -- If we're multiple steps from the target continue moving to the target
        else [model {selfState = MovingTo tgt (remaining - 1)}]
    OpeningValve curr
      -- Finished opening a valve so consider all possible next moves
        -- Options of continuing to next node
     ->
      (graph model ! curr & links & Map.toList &
       filter (\(k, _) -> k `notElem` valvesVisited model) &
       map
         (\(newTgt, newCost) ->
            model
              { selfState = MovingTo newTgt newCost
              , valvesVisited = newTgt : valvesVisited model
              , valvesOpened = curr : valvesOpened model
              }))
        -- Option of walking around in a circle for the rest of time
       ++
      [ model
          { selfState = MovingTo curr 500
          , valvesOpened = curr : valvesOpened model
          }
      ]

sim :: Model -> Int
sim model =
  if timeRemaining model <= 0
    then preasureRelieved model
    else do
      maximum .
        map
          (\m ->
             sim
               (m
                  { timeRemaining = timeRemaining m - 1
                  , preasureRelieved = preasureRelieved m + rate m
                  })) .
        updateSelf $
        model

parseNode :: String -> (String, Node)
parseNode str = do
  let name = words str !! 1
  let vlue = read . init . drop 5 $ (words str !! 4)
  let lnks = fromList . map (\s -> (init s, 1)) . drop 9 . words $ (str ++ ",")
  (name, Node {value = vlue, links = lnks})

parseInput :: String -> Graph
parseInput = fromList . map parseNode . lines

reduceNode :: String -> Graph -> Graph
reduceNode name grph = do
  let lnks = links (grph ! name)
  Map.mapWithKey
    (\self node ->
       Node
         { value = value node
         , links =
             do let prevLinks = links node
                case Map.lookup name prevLinks of
                  Just cost ->
                    Map.delete self $
                    unionWith
                      min
                      (Map.map (+ cost) lnks)
                      (Map.delete name prevLinks)
                  Nothing -> prevLinks
         })
    grph

connectAll :: Graph -> Graph
connectAll grph = do
  let keys = Map.keys grph
  let done =
        all
          (\(name, node) ->
             sort (Map.keys (links node)) == sort (delete name keys)) $
        Map.toList grph
  if done
    then grph
    else connectAll $
         foldl
           (\gr key -> do
              let lnks = links (gr ! key)
              Map.mapWithKey
                (\k node ->
                   if k == key
                     then node
                     else case Map.lookup key (links node) of
                            Just cost ->
                              Node
                                { value = value node
                                , links =
                                    do let currLinks = links node
                                       let newLinks =
                                             Map.delete k $
                                             Map.map (+ cost) lnks
                                       unionWith min currLinks newLinks
                                }
                            Nothing -> node)
                gr)
           grph
           keys

reduceZeroes :: Graph -> Graph
reduceZeroes grph = do
  let zeroNodes =
        map fst . filter (\(_, node) -> value node == 0) . toList $ grph
  foldr reduceNode grph zeroNodes

day16'1 :: String -> Int
day16'1 input =
  sim
    (Model
       { graph = reduceZeroes . connectAll . parseInput $ input
       , valvesVisited = ["AA"]
       , valvesOpened = []
       , selfState = OpeningValve "AA"
       , elephantState = MovingTo "AA" 500
       , preasureRelieved = 0
       , timeRemaining = 30
       })

day16'2 :: String -> Int
day16'2 = length

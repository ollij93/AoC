module AoC2022.Day16
  ( day16'1
  , day16'2
  ) where

import           Data.Function ((&))
import           Data.HashMap  (fromList, toList, unionWith, (!))
import qualified Data.HashMap  as Map
import           Data.List     (delete, sort)
import           Debug.Trace   (trace)
import           Util          (dbg)

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

updateActor :: (Model -> ActorState) -> Model -> [(ActorState, Model)]
updateActor state model =
  case state model of
    MovingTo tgt remaining ->
      if remaining <= 1
        -- If we're only one step from the target start opening the valve in the next minute
        then [(OpeningValve tgt, model)]
        -- If we're multiple steps from the target continue moving to the target
        else [(MovingTo tgt (remaining - 1), model)]
    OpeningValve curr
      -- Finished opening a valve so consider all possible next moves
     -> do
      let walks =
            graph model ! curr & links & Map.toList &
            filter
              (\(k, c) ->
                 c + 1 < timeRemaining model && k `notElem` valvesVisited model) &
            map
              (\(newTgt, newCost) ->
                 ( MovingTo newTgt newCost
                 , model
                     { valvesVisited = newTgt : valvesVisited model
                     , valvesOpened = curr : valvesOpened model
                     }))
      if null walks
        -- Option of walking around in a circle for the rest of time
        then [ ( MovingTo curr 500
               , model {valvesOpened = curr : valvesOpened model})
             ]
        -- Options of continuing to next node
        else walks

updateSelf :: Model -> [Model]
updateSelf =
  map (\(state, model) -> model {selfState = state}) . updateActor selfState

updateElephant :: Model -> [Model]
updateElephant =
  map (\(state, model) -> model {elephantState = state}) .
  updateActor elephantState

sim :: Model -> Int
sim model =
  if timeRemaining model <= 0
    then preasureRelieved model
    else do
      maximum .
        map
          (\m -> do
             let escape =
                   case selfState m of
                     OpeningValve _ -> False
                     MovingTo _ sN ->
                       sN > timeRemaining model &&
                       case elephantState m of
                         OpeningValve _ -> False
                         MovingTo _ eN  -> eN > timeRemaining model
             if escape
               then preasureRelieved m + (rate m * timeRemaining m)
               else dbg (show $ valvesOpened m) $
                    sim
                      (m
                         { timeRemaining = timeRemaining m - 1
                         , preasureRelieved = preasureRelieved m + rate m
                         })) .
        concatMap updateElephant . updateSelf $
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
day16'2 input =
  sim
    (Model
       { graph = reduceZeroes . connectAll . parseInput $ input
       , valvesVisited = ["AA"]
       , valvesOpened = []
       , selfState = OpeningValve "AA"
       , elephantState = OpeningValve "AA"
       , preasureRelieved = 0
       , timeRemaining = 26
       })

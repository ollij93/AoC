module AoC2022.Day16
  ( day16'1
  , day16'2
  ) where

import           Data.HashMap (fromList, toList, unionWith, (!))
import qualified Data.HashMap as Map
import           Data.List    (delete, permutations, sort, (\\))
import           Debug.Trace  (trace)
import           Util         (dbg)

data Node =
  Node
    { value :: Int
    , links :: Map.Map String Int
    }

instance Show Node where
  show n = "Node(value=" ++ show (value n) ++ ",links=" ++ show (links n) ++ ")"

type Graph = Map.Map String Node

parseNode :: String -> (String, Node)
parseNode s = do
  let name = words s !! 1
  let vlue = read . init . drop 5 $ (words s !! 4)
  let lnks = fromList . map (\s -> (init s, 1)) . drop 9 . words $ (s ++ ",")
  (name, Node {value = vlue, links = lnks})

parseInput :: String -> Graph
parseInput = fromList . map parseNode . lines

reduceNode :: String -> Graph -> Graph
reduceNode name graph = do
  let lnks = links (graph ! name)
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
    graph

connectAll :: Graph -> Graph
connectAll graph = do
  let keys = Map.keys graph
  let done =
        all
          (\(name, node) ->
             sort (Map.keys (links node)) == sort (delete name keys)) $
        Map.toList graph
  if done
    then graph
    else connectAll $
         foldl
           (\grph key -> do
              let lnks = links (graph ! key)
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
                grph)
           graph
           keys

reduceZeroes :: Graph -> Graph
reduceZeroes graph = do
  let zeroNodes =
        map fst . filter (\(k, node) -> value node == 0) . toList $ graph
  foldr reduceNode graph zeroNodes

nonZeroKeys :: Graph -> [String]
nonZeroKeys = map fst . filter (\(_, node) -> value node > 0) . Map.toList

possibleOrders :: Graph -> [[String]]
possibleOrders = permutations . nonZeroKeys

journey :: [String] -> [(String, String)]
journey l = zip l (drop 1 l)

runJourney :: Int -> Graph -> [(String, String)] -> Int
runJourney timeRemaining graph steps =
  if timeRemaining <= 0
    then 0
    else case steps of
           [] -> 0
           (at, next):rest -> do
             let val = value (graph ! at)
             let stepCost = links (graph ! at) ! next
             let timeAtNext =
                   timeRemaining -
                   (if val > 0
                      then 1
                      else 0) -
                   stepCost
             (timeRemaining - 1) * val +
               case rest of
                 [] ->
                   if timeAtNext > 0
                     then (timeAtNext - 1) * value (graph ! next)
                     else 0
                 _ -> runJourney timeAtNext graph rest

explore :: Int -> Graph -> [String] -> Int
explore timeRemaining graph stack =
  if timeRemaining <= 0
    then 0
    else do
      let currentNodeName = head stack
      let val = value (graph ! currentNodeName)
      let nexts =
            filter (\(k, v) -> v < timeRemaining && k `notElem` stack) .
            Map.toList . links $
            graph ! currentNodeName
      maximum
        (val * (timeRemaining - 1) :
         map
           (\(nextName, nextCost) -> do
              let timeAtNext =
                    timeRemaining -
                    (if val > 0
                       then 1
                       else 0) -
                    nextCost
              (val * (timeRemaining - 1)) +
                explore timeAtNext graph (nextName : stack))
           nexts)

day16'1 :: String -> Int
day16'1 input = do
  let graph =
        Map.mapWithKey dbg . reduceZeroes . connectAll . parseInput $ input
  explore 30 graph ["AA"]

--  let orders = possibleOrders graph
--  maximum $
--    map
--      (\order -> dbg (show order) $ runJourney 30 graph . journey $ "AA" : order)
--      orders
day16'2 :: String -> Int
day16'2 = length

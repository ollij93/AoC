module AoC2022.Day7
  ( day7'1
  , day7'2
  ) where

import           Data.HashMap    (Map, empty, insert, keys, (!))
import           Data.List.Split (splitOn)
import           Util

data Cmd
  = CD String
  | LS [String] [(String, Int)]

parseFileLine :: String -> (String, Int)
parseFileLine s = do
  let (size, name) = splitOnce ' ' s
  (name, read size)

parseLS :: [String] -> Cmd
parseLS =
  uncurry LS .
  foldl
    (\(ds, fs) l ->
       case take 3 l of
         "dir" -> (drop 4 l : ds, fs)
         _     -> (ds, parseFileLine l : fs))
    ([], [])

parseCmd :: String -> Cmd
parseCmd s =
  case take 2 s of
    "cd" -> CD $ drop 3 s
    _    -> parseLS . drop 1 . lines $ s

parseCmds :: String -> [Cmd]
parseCmds = map parseCmd . splitOn "\n$ " . drop 2

data FSEntry
  = Dir [String]
  | File Int

instance Show FSEntry where
  show x =
    case x of
      Dir dirs -> "Dir " ++ show dirs
      File n   -> "File " ++ show n

type FSTree = Map String FSEntry

joinPath :: String -> String -> String
joinPath parent name =
  case parent of
    "/" -> '/' : name
    _   -> parent ++ "/" ++ name

updateFromLS :: String -> [String] -> [(String, Int)] -> FSTree -> FSTree
updateFromLS cwd dirs files tree =
  insert cwd (Dir . map (joinPath cwd) $ dirs ++ map fst files) $
  foldl (\m (f, s) -> insert (joinPath cwd f) (File s) m) tree files

parentDir :: String -> String
parentDir = init . concatMap (++ "/") . init . splitOn "/"

updateFromCmd :: (String, FSTree) -> Cmd -> (String, FSTree)
updateFromCmd (cwd, tree) cmd =
  case cmd of
    CD dir ->
      case dir of
        ".." -> (parentDir cwd, tree)
        "/"  -> ("/", tree)
        _    -> (joinPath cwd dir, tree)
    LS dirs files -> (cwd, updateFromLS cwd dirs files tree)

toTree :: [Cmd] -> FSTree
toTree = snd . foldl updateFromCmd ("", empty)

getSize :: FSTree -> String -> Int
getSize tree path =
  case tree ! path of
    File s      -> s
    Dir entries -> sum . map (getSize tree) $ entries

filterDirSizes :: (Int -> Bool) -> FSTree -> [(String, Int)]
filterDirSizes requirement tree =
  filter
    (\(p, size) ->
       case tree ! p of
         Dir _ -> requirement size
         _     -> False) .
  map (\p -> (p, getSize tree p)) $
  keys tree

day7'1 :: String -> Int
day7'1 = sum . map snd . filterDirSizes (<= 100000) . toTree . parseCmds

getDirsToFreeSpace :: Int -> Int -> FSTree -> [(String, Int)]
getDirsToFreeSpace required total tree = do
  let current = getSize tree "/"
  let toFree = current - (total - required)
  filterDirSizes (>= toFree) tree

day7'2 :: String -> Int
day7'2 =
  minimum . map snd . getDirsToFreeSpace 30000000 70000000 . toTree . parseCmds
--
-- prettyPrintPath :: String -> FSTree -> [String]
-- prettyPrintPath path tree =
--   case trace (show $ keys tree) (tree ! path) of
--     File size -> ["- " ++ path ++ " " ++ show size]
--     Dir contents ->
--       ("- " ++ path ++ " (dir)") :
--       foldr
--         (\p c -> map ("  " ++) (prettyPrintPath p tree) ++ c)
--         []
--         (sort contents)
--
-- prettyPrint :: FSTree -> String
-- prettyPrint = unlines . prettyPrintPath "/"
--

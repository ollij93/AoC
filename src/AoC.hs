module AoC
  ( Solution(..)
  , process
  ) where

-- Solution registry
data Solution =
  Solution
    { name     :: String
    , testPath :: String
    , dataPath :: String
    , fnc      :: String -> Int
    }

-- Run functions
runSolution :: Solution -> String -> String
runSolution Solution {name = sName, fnc = sFnc} input =
  sName ++ ": " ++ show (sFnc input) ++ "\n"

readAndRun :: (Solution -> FilePath) -> Solution -> IO String
readAndRun pathSelector soln =
  fmap (runSolution soln) $ readFile $ pathSelector soln

process :: (Solution -> FilePath) -> [Solution] -> IO ()
process pathSelector solns = do
  result <- concat <$> mapM (readAndRun pathSelector) solns
  putStrLn result

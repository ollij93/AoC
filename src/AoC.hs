module AoC
  ( Solution(..)
  , process
  ) where

-- Solution registry
data Solution
  = ISolution
      { name     :: String
      , testPath :: String
      , dataPath :: String
      , ifnc      :: String -> Int
      }
  | SSolution
      { name     :: String
      , testPath :: String
      , dataPath :: String
      , sfnc      :: String -> String
      }

-- Run functions
runSolution :: Solution -> String -> String
runSolution soln input =
  case soln of
    ISolution {name = sName, ifnc = iFnc} ->
      sName ++ ": " ++ show (iFnc input) ++ "\n"
    SSolution {name = sName, sfnc = sFnc} -> sName ++ ": " ++ sFnc input ++ "\n"

readAndRun :: (Solution -> FilePath) -> Solution -> IO String
readAndRun pathSelector soln =
  fmap (runSolution soln) $ readFile $ pathSelector soln

process :: (Solution -> FilePath) -> [Solution] -> IO ()
process pathSelector solns = do
  result <- concat <$> mapM (readAndRun pathSelector) solns
  putStrLn result

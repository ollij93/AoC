module Main
  ( main
  ) where

import           Lib

runSolution :: String -> (String, String -> Int) -> String
runSolution input (name, fnc) = name ++ ": " ++ show (fnc input)

main :: IO ()
main = do
  content <- getContents
  putStrLn $
    foldl
      (\s v ->
         s ++
         (if isEmpty s
            then ""
            else "\n") ++
         runSolution content v)
      ""
      solutions

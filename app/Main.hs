module Main
  ( main
  ) where

import           Lib (Solution (dataPath), process, solutions)
import           Lib2021 (solutions2021)

main :: IO ()
main = process dataPath $ solutions2021 ++ solutions

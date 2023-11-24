module Main
  ( main
  ) where

import           AoC     (Solution (dataPath), process)
import qualified AoC2021
import qualified AoC2022

main :: IO ()
main = process dataPath $ AoC2021.solutions ++ AoC2022.solutions

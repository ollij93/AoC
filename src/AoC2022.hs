module AoC2022
  ( solutions
  ) where

import           AoC             (Solution (..))
import           AoC2022.Day1    (day1'1, day1'2)
import           AoC2022.Day2    (day2'1, day2'1ascii, day2'2, day2'2ascii)
import           AoC2022.Day3    (day3'1, day3'2)


-- Solution registry
solutions :: [Solution]
solutions =
  [ Solution
      { name = "Day1.1"
      , testPath = "inputs/2022/tests/day1.txt"
      , dataPath = "inputs/2022/day1.txt"
      , fnc = day1'1
      }
  , Solution
      { name = "Day1.2"
      , testPath = "inputs/2022/tests/day1.txt"
      , dataPath = "inputs/2022/day1.txt"
      , fnc = day1'2
      }
  , Solution
      { name = "Day2.1"
      , testPath = "inputs/2022/tests/day2.txt"
      , dataPath = "inputs/2022/day2.txt"
      , fnc = day2'1
      }
  , Solution
      { name = "Day2.2"
      , testPath = "inputs/2022/tests/day2.txt"
      , dataPath = "inputs/2022/day2.txt"
      , fnc = day2'2
      }
  , Solution
      { name = "Day2.1 (ascii)"
      , testPath = "inputs/2022/tests/day2.txt"
      , dataPath = "inputs/2022/day2.txt"
      , fnc = day2'1ascii
      }
  , Solution
      { name = "Day2.2 (ascii)"
      , testPath = "inputs/2022/tests/day2.txt"
      , dataPath = "inputs/2022/day2.txt"
      , fnc = day2'2ascii
      }
  , Solution
      { name = "Day3.1"
      , testPath = "inputs/2022/tests/day3.txt"
      , dataPath = "inputs/2022/day3.txt"
      , fnc = day3'1
      }
  , Solution
      { name = "Day3.2"
      , testPath = "inputs/2022/tests/day3.txt"
      , dataPath = "inputs/2022/day3.txt"
      , fnc = day3'2
      }
  ]

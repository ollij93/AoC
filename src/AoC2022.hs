module AoC2022
  ( solutions
  ) where

import           AoC             (Solution (..))
import           AoC2022.Day1    (day1'1, day1'2)
import           AoC2022.Day2    (day2'1, day2'1ascii, day2'2, day2'2ascii)
import           AoC2022.Day3    (day3'1, day3'2)
import           AoC2022.Day4    (day4'1, day4'2)


-- Solution registry
solutions :: [Solution]
solutions =
  [ ISolution
      { name = "Day1.1"
      , testPath = "inputs/2022/tests/day1.txt"
      , dataPath = "inputs/2022/day1.txt"
      , ifnc = day1'1
      }
  , ISolution
      { name = "Day1.2"
      , testPath = "inputs/2022/tests/day1.txt"
      , dataPath = "inputs/2022/day1.txt"
      , ifnc = day1'2
      }
  , ISolution
      { name = "Day2.1"
      , testPath = "inputs/2022/tests/day2.txt"
      , dataPath = "inputs/2022/day2.txt"
      , ifnc = day2'1
      }
  , ISolution
      { name = "Day2.2"
      , testPath = "inputs/2022/tests/day2.txt"
      , dataPath = "inputs/2022/day2.txt"
      , ifnc = day2'2
      }
  , ISolution
      { name = "Day2.1 (ascii)"
      , testPath = "inputs/2022/tests/day2.txt"
      , dataPath = "inputs/2022/day2.txt"
      , ifnc = day2'1ascii
      }
  , ISolution
      { name = "Day2.2 (ascii)"
      , testPath = "inputs/2022/tests/day2.txt"
      , dataPath = "inputs/2022/day2.txt"
      , ifnc = day2'2ascii
      }
  , ISolution
      { name = "Day3.1"
      , testPath = "inputs/2022/tests/day3.txt"
      , dataPath = "inputs/2022/day3.txt"
      , ifnc = day3'1
      }
  , ISolution
      { name = "Day3.2"
      , testPath = "inputs/2022/tests/day3.txt"
      , dataPath = "inputs/2022/day3.txt"
      , ifnc = day3'2
      }
  , ISolution
      { name = "Day4.1"
      , testPath = "inputs/2022/tests/day4.txt"
      , dataPath = "inputs/2022/day4.txt"
      , ifnc = day4'1
      }
   , ISolution
       { name = "Day4.2"
       , testPath = "inputs/2022/tests/day4.txt"
       , dataPath = "inputs/2022/day4.txt"
       , ifnc = day4'2
       }
  ]

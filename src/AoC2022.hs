module AoC2022
  ( solutions
  ) where

import           AoC           (Solution (..))
import           AoC2022.Day01 (day1'1, day1'2)
import           AoC2022.Day02 (day2'1, day2'1ascii, day2'2, day2'2ascii)
import           AoC2022.Day03 (day3'1, day3'2)
import           AoC2022.Day04 (day4'1, day4'2)
import           AoC2022.Day05 (day5'1, day5'2)
import           AoC2022.Day06 (day6'1, day6'2)
import           AoC2022.Day07 (day7'1, day7'2)
import           AoC2022.Day08 (day8'1, day8'2)
import           AoC2022.Day09 (day9'1, day9'2)
import           AoC2022.Day10 (day10'1, day10'2)
import           AoC2022.Day11 (day11'1, day11'2)
import           AoC2022.Day12 (day12'1, day12'2)
import           AoC2022.Day13 (day13'1, day13'2)
import           AoC2022.Day14 (day14'1, day14'2)
import           AoC2022.Day15 (day15'1, day15'2)
import           AoC2022.Day16 (day16'1, day16'2)
import           AoC2022.Day17 (day17'1, day17'2)

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
  , SSolution
      { name = "Day5.1"
      , testPath = "inputs/2022/tests/day5.txt"
      , dataPath = "inputs/2022/day5.txt"
      , sfnc = day5'1
      }
  , SSolution
      { name = "Day5.2"
      , testPath = "inputs/2022/tests/day5.txt"
      , dataPath = "inputs/2022/day5.txt"
      , sfnc = day5'2
      }
  , SSolution
      { name = "Day6.1"
      , testPath = "inputs/2022/tests/day6.txt"
      , dataPath = "inputs/2022/day6.txt"
      , sfnc = day6'1
      }
  , SSolution
      { name = "Day6.2"
      , testPath = "inputs/2022/tests/day6.txt"
      , dataPath = "inputs/2022/day6.txt"
      , sfnc = day6'2
      }
  , ISolution
      { name = "Day7.1"
      , testPath = "inputs/2022/tests/day7.txt"
      , dataPath = "inputs/2022/day7.txt"
      , ifnc = day7'1
      }
  , ISolution
      { name = "Day7.2"
      , testPath = "inputs/2022/tests/day7.txt"
      , dataPath = "inputs/2022/day7.txt"
      , ifnc = day7'2
      }
  , ISolution
      { name = "Day8.1"
      , testPath = "inputs/2022/tests/day8.txt"
      , dataPath = "inputs/2022/day8.txt"
      , ifnc = day8'1
      }
  , ISolution
      { name = "Day8.2"
      , testPath = "inputs/2022/tests/day8.txt"
      , dataPath = "inputs/2022/day8.txt"
      , ifnc = day8'2
      }
  , ISolution
      { name = "Day9.1"
      , testPath = "inputs/2022/tests/day9.txt"
      , dataPath = "inputs/2022/day9.txt"
      , ifnc = day9'1
      }
  , ISolution
      { name = "Day9.2"
      , testPath = "inputs/2022/tests/day9_2.txt"
      , dataPath = "inputs/2022/day9.txt"
      , ifnc = day9'2
      }
  , ISolution
      { name = "Day10.1"
      , testPath = "inputs/2022/tests/day10.txt"
      , dataPath = "inputs/2022/day10.txt"
      , ifnc = day10'1
      }
  , SSolution
      { name = "Day10.2"
      , testPath = "inputs/2022/tests/day10.txt"
      , dataPath = "inputs/2022/day10.txt"
      , sfnc = day10'2
      }
  , ISolution
      { name = "Day11.1"
      , testPath = "inputs/2022/tests/day11.txt"
      , dataPath = "inputs/2022/day11.txt"
      , ifnc = day11'1
      }
  , ISolution
      { name = "Day11.2"
      , testPath = "inputs/2022/tests/day11.txt"
      , dataPath = "inputs/2022/day11.txt"
      , ifnc = day11'2
      }
  , ISolution
      { name = "Day12.1"
      , testPath = "inputs/2022/tests/day12.txt"
      , dataPath = "inputs/2022/day12.txt"
      , ifnc = day12'1
      }
  , ISolution
      { name = "Day12.2"
      , testPath = "inputs/2022/tests/day12.txt"
      , dataPath = "inputs/2022/day12.txt"
      , ifnc = day12'2
      }
  , ISolution
      { name = "Day13.1"
      , testPath = "inputs/2022/tests/day13.txt"
      , dataPath = "inputs/2022/day13.txt"
      , ifnc = day13'1
      }
  , ISolution
      { name = "Day13.2"
      , testPath = "inputs/2022/tests/day13.txt"
      , dataPath = "inputs/2022/day13.txt"
      , ifnc = day13'2
      }
  , ISolution
      { name = "Day14.1"
      , testPath = "inputs/2022/tests/day14.txt"
      , dataPath = "inputs/2022/day14.txt"
      , ifnc = day14'1
      }
  , ISolution
      { name = "Day14.2"
      , testPath = "inputs/2022/tests/day14.txt"
      , dataPath = "inputs/2022/day14.txt"
      , ifnc = day14'2
      }
  , ISolution
      { name = "Day15.1"
      , testPath = "inputs/2022/tests/day15.txt"
      , dataPath = "inputs/2022/day15.txt"
      , ifnc = day15'1
      }
  , ISolution
      { name = "Day15.2"
      , testPath = "inputs/2022/tests/day15.txt"
      , dataPath = "inputs/2022/day15.txt"
      , ifnc = day15'2
      }
  , ISolution
      { name = "Day16.1"
      , testPath = "inputs/2022/tests/day16.txt"
      , dataPath = "inputs/2022/day16.txt"
      , ifnc = day16'1
      }
--   , ISolution
--       { name = "Day16.2"
--       , testPath = "inputs/2022/tests/day16.txt"
--       , dataPath = "inputs/2022/day16.txt"
--       , ifnc = day16'2
--       }
  , ISolution
      { name = "Day17.1"
      , testPath = "inputs/2022/tests/day17.txt"
      , dataPath = "inputs/2022/day17.txt"
      , ifnc = day17'1
      }
--   , ISolution
--       { name = "Day17.2"
--       , testPath = "inputs/2022/tests/day17.txt"
--       , dataPath = "inputs/2022/day17.txt"
--       , ifnc = day17'2
--       }
  ]

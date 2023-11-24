import           AoC             (Solution (testPath), process)
import qualified AoC2021         (solutions)
import qualified AoC2022         (solutions)

main :: IO ()
main = process testPath $ AoC2021.solutions ++ AoC2022.solutions

import           Lib     (Solution (testPath), process, solutions)
import           Lib2021 (solutions2021, groups)

main :: IO ()
main = process testPath $ solutions2021 ++ solutions

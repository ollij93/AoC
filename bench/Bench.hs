import           AoC                         (Solution (..))
import qualified AoC2021
import qualified AoC2022
import           Criterion.Main              (bench, bgroup, defaultMain, nfIO)
import           Criterion.Measurement.Types (Benchmark)

solutionBench :: Solution -> Benchmark
solutionBench Solution {name = sName, dataPath = sPath, fnc = sFnc} =
  bench sName $ nfIO $ fmap sFnc $ readFile sPath

benches2021 :: [Benchmark]
benches2021 = map solutionBench AoC2021.solutions

benches2022 :: [Benchmark]
benches2022 = map solutionBench AoC2022.solutions

main :: IO ()
main = defaultMain [bgroup "2021" benches2021, bgroup "2022" benches2022]

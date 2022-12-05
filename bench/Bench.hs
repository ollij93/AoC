import           AoC                         (Solution (..))
import qualified AoC2021
import qualified AoC2022
import           Criterion.Main              (bench, bgroup, defaultMain, nfIO)
import           Criterion.Measurement.Types (Benchmark)

solutionBench :: Solution -> Benchmark
solutionBench soln =
  case soln of
    ISolution {name = sName, dataPath = sPath, ifnc = iFnc} ->
      bench sName $ nfIO $ fmap iFnc $ readFile sPath
    SSolution {name = sName, dataPath = sPath, sfnc = sFnc} ->
      bench sName $ nfIO $ fmap sFnc $ readFile sPath

benches2021 :: [Benchmark]
benches2021 = map solutionBench AoC2021.solutions

benches2022 :: [Benchmark]
benches2022 = map solutionBench AoC2022.solutions

main :: IO ()
main = defaultMain [bgroup "2021" benches2021, bgroup "2022" benches2022]

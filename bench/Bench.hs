import           Criterion.Main              (bench, bgroup, defaultMain, nfIO)
import           Criterion.Measurement.Types (Benchmark)
import           Lib                         (Solution (..), process, solutions)
import           Lib2021                     (solutions2021)

solutionBench :: Solution -> Benchmark
solutionBench Solution {name = sName, dataPath = sPath, fnc = sFnc} =
  bench sName $ nfIO $ fmap sFnc $ readFile sPath

benches2021 :: [Benchmark]
benches2021 = map solutionBench solutions2021

benches2022 :: [Benchmark]
benches2022 = map solutionBench solutions

main :: IO ()
main = defaultMain [bgroup "2021" benches2021, bgroup "2022" benches2022]

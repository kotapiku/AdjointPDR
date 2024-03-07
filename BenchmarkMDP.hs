import           AdjointPDR
import           Benchmarks
import qualified Data.IntMap      as IM
import           Data.List        (nub, union)
import           ForMDP
import           ForMDPS
import           Test.Tasty.Bench


type RunBench p n = Bench -> Rational -> IO (PDRAnswer p n)

reachable :: Delta a -> (Int -> Bool) -> [Int] -> [Int]
reachable dlt isSafe ls =
  let next = filter isSafe $ union ls $ nub $ concatMap (concatMap IM.keys . dlt) ls in
    if length next == length ls then ls else reachable dlt isSafe next

hBenchmark :: RunBench p n -> (String -> String -> Rational -> Bool) -> Bench -> Benchmark
hBenchmark run notTO xs@(name, fileName, answer, sd, lambdas) =
  bgroup (fileName ++ "(Pr_max=" ++ answer ++ ")") $ map h $ filter (notTO name fileName) lambdas
  where
    h lambda = bench ("lambda=" ++ show lambda) $ whnfIO $ run xs lambda

runMC :: (ProbMap Rational -> Int -> Problem (ProbMap Rational) -> Memo (ProbMap Rational) Int -> IO (ProbMap Rational, Memo (ProbMap Rational) Int)) -> RunBench (ProbMap Rational) Int
runMC fco (_, _, _, sd, _) lambda = do
  adjointPDR defaultOpt{optPrint=PrintAll} (funcSettingMCS sd){fConflict=fco} Problem{b=fMCS sd, safeElem=ProbMap 1 $ IM.singleton (initialN sd) lambda}

runMDP :: (ProbMap Rational -> ProbMap Rational -> Problem (ProbMap Rational) -> Memo (ProbMap Rational) (ProbMap Rational) -> IO (ProbMap Rational, Memo (ProbMap Rational) (ProbMap Rational))) -> RunBench (ProbMap Rational) (ProbMap Rational)
runMDP fco (_, _, _, sd, _) lambda = do
  let xs = reachable (convertDelta sd) (\s -> not $ checkFml (convertNtoM (vars sd) s) $ bad sd) [initialN sd]
  adjointPDR defaultOpt (funcSettingS sd){fConflict=fco} Problem{b=fS' xs sd, safeElem=ProbMap 1 $ IM.singleton (initialN sd) lambda}

runBenchmark :: RunBench p n -> (String -> String -> Rational -> Bool) -> Bench -> IO ()
runBenchmark run notTO xs@(name, fileName, answer, sd, lambdas) = do
  mapM_ (\lambda -> do
    putStr $ fileName ++ "(Pr_max=" ++ answer ++ ")" ++ "(lambda=" ++ show lambda ++ "): "
    print . isValid =<< run xs lambda) $ filter (notTO name fileName) lambdas

-- for test
isValid :: PDRAnswer p n -> String
isValid (Valid _)   = "Valid"
isValid (InValid _) = "InValid"

allBenchmarkMC :: [Bench]
allBenchmarkMC = gridBenchmark ++ brpBenchmark ++ zeroconfBenchmark ++ chainBenchmark ++ doubleChainBenchmark ++ haddadMonmegeBenchmark

allBenchmark :: [Bench]
allBenchmark = cDrive2Benchmark ++ tireWorldBenchmark

-- functions to filter out time out benchmarks
notTOInit :: String -> String -> Rational -> Bool
notTOInit "ZeroConf" _ lambda      = lambda == 0.45
notTOInit "HaddadMonmege" _ lambda = False
notTOInit _ _ _                    = True

notTO01MC :: String -> String -> Rational -> Bool
notTO01MC _ "zero_conf_medium.pm" lambda = lambda == 0.45
notTO01MC _ "chain_small.pm" lambda      = lambda == 0.3
notTO01MC _ _ _                          = True

notTOBMC :: String -> String -> Rational -> Bool
notTOBMC _ "zero_conf_medium.pm" lambda = lambda == 0.45
notTOBMC _ _ _                          = True

notTOMCS :: String -> String -> Rational -> Bool
notTOMCS _ "zero_conf_medium.pm" lambda = lambda /= 0.52
notTOMCS "DoubleChain" _ _              = False
notTOMCS _ _ _                          = True

notTO01 :: String -> String -> Rational -> Bool
notTO01 _ _ _ = True

notTOB :: String -> String -> Rational -> Bool
notTOB "CDrive2" _ lambda   = lambda == 0.5
notTOB "TireWorld" _ lambda = lambda == 0.2

notTOS :: String -> String -> Rational -> Bool
notTOS "CDrive2" _ lambda = lambda == 0.5
notTOS _ _ _              = False

getSD :: Bench -> DeltaS Rational
getSD (_, _, _, sd, _) = sd


main :: IO ()
main = do
  -- for mc
  -- defaultMain $ map (hBenchmark (runMC hCoBMC) notTOBMC) allBenchmarkMC
  -- defaultMain $ map (hBenchmark (runMC hCo01MC) notTO01MC) allBenchmarkMC
  -- defaultMain $ map (\b -> hBenchmark (runMC $ hCoMCS $ getSD b) notTOMCS b) haddadMonmegeBenchmark

  -- for mdp
  -- defaultMain $ map (hBenchmark (runMDP hCoB) notTOB) allBenchmark
  -- defaultMain $ map (hBenchmark (runMDP hCo01) notTO01) allBenchmark
  -- defaultMain $ map (\b -> hBenchmark (runMDP $ hCoS $ getSD b) notTOS b) allBenchmark

  -- to run individual benchmarks
  print . isValid =<< runMC hCo01MC (ijBenchmark!!0) 0.5 -- InValid
  -- print . isValid =<< (runMC $ hCoMCS $ getSD $ haddadMonmegeBenchmark!!1) (haddadMonmegeBenchmark!!0) 0.9
  -- print . isValid =<< (runMDP hCoB) (tireWorldBenchmark!!0) 0.5
  -- print . isValid =<< (runMDP $ hCoS $ getSD $ cDrive2Benchmark!!0) (cDrive2Benchmark!!0) 0.5
  -- mapM_ (runBenchmark (runMC hCoBMC) notTOBMC) allBenchmark

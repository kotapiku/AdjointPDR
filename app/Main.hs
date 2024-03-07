{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Main (main) where

import           Benchmarks
import qualified Data.IntMap as IM
import           Data.List
import qualified Data.Map    as M
import           ForMDP
import           ForMDPS
import           AdjointPDR



-- for test
isValid :: PDRAnswer p n -> String
isValid (Valid _)   = "Valid"
isValid (InValid _) = "InValid"

main :: IO ()
main = do
  let tireWorld = tireWorldBenchmark!!0
  print . isValid =<< adjointPDR defaultOpt{optPrint = PrintRule, optMaxStep = Nothing} (funcSettingS tireWorld){fConflict=hCoMeetS}  Problem{b=fS tireWorld, safeElem=ProbMap 1 $ IM.singleton (initialN tireWorld) 0.99}
  -- print . isValid =<< ltPDRadj defaultOpt{optPrint = PrintLength, optMaxStep = Nothing} (funcSettingS cDrive2){fConflict=hCoMeetMDP}  Problem{fa=fS cDrive2, initElem=ProbMap 0 IM.empty, safeElem=ProbMap 1 $ IM.singleton (initialN cDrive2) 0.99}
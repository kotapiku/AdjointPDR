{-# LANGUAGE TypeFamilies #-}
module MDPTest where

import           Data.IntMap      (fromList)
import qualified Data.IntMap      as Map
import           ForMDP
import           AdjointPDR
import           Test.Tasty.HUnit


stateNum :: Int
stateNum = 6

delta :: Delta Rational
delta 0 = [Map.fromList [(1, 0.5), (2, 0.5)]]
delta 1 = [Map.fromList [(0, 0.5), (3, 0.5)]]
delta 2 = [Map.fromList [(4, 0.5), (5, 0.5)], Map.fromList [(1, 1)]]
delta 3 = [Map.fromList [(4, 1/3), (5, 2/3)]]
delta 4 = [Map.fromList [(4, 1)]]
delta 5 = [Map.fromList [(5, 1)]]
delta _ = []


-- for test
isValid :: PDRAnswer p n -> Bool
isValid (Valid _)   = True
isValid (InValid _) = False

problem :: Rational -> Problem (ProbMap Rational)
problem lambda = Problem{b=f stateNum delta (== 5), safeElem=ProbMap 1 $ Map.singleton 0 lambda}

unit_mdp_case1 :: IO ()
unit_mdp_case1 = adjointPDR defaultOpt (funcSetting delta (== 5)) (problem 0.5) >>= (assertBool "an example in PrIC3 paper" . not) . isValid -- pr=2/3

unit_mdp_case2 :: IO ()
unit_mdp_case2 = adjointPDR defaultOpt (funcSetting delta (== 5)) (problem 0.9) >>= assertBool "an example in PrIC3 paper" . isValid -- pr=2/3

unit_corners_case1 :: IO ()
unit_corners_case1 = assertEqual "test1 for cornerPoints" [fromList [(1,0 / 1)],fromList [(2,0 / 1)]] $ cornerPoints $ ProbMap (1/2) $ Map.fromList [(1, 1/2), (2, 1/2)]

unit_corners_case2 :: IO ()
unit_corners_case2 = assertEqual "test2 for cornerPoints" [fromList [(0,0 / 1),(1,0 / 1),(2,2 / 3)],fromList [(0,0 / 1),(1,2 / 3),(2,0 / 1)],fromList [(1,0 / 1),(2,1 / 6)],fromList [(1,1 / 6),(2,0 / 1)]] $ cornerPoints $ ProbMap (1/3) $ Map.fromList [(0, 1/4), (1, 1/2), (2, 1/2)]
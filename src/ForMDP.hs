{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TupleSections         #-}
module ForMDP (
  getMap,
  valueProbMap,
  Delta,
  f,
  f',
  fMC,
  ProbMap (..),
  cornerPoints,
  cornerPoints',
  fromCornerToMap,
  hCornerPoints,
  maximumSafe,
  funcSetting,
  funcSettingMC,
  hCa,
  hDe,
  hCoInitMC,
  hCoMeet01MC,
  hCoMeetBMC,
  hCoMeet01,
  hCoMeetB,
  DeltaMC,
) where

import           Data.Bifunctor (first)
import           Data.IntMap    (IntMap, (!))
import qualified Data.IntMap    as IM
import           Data.List      (nub, find, maximumBy)
import qualified Data.Map       as M
import           Data.Maybe     (isJust)
import           Data.Ord       (comparing)
import           AdjointPDR

-- ProbMap n f = (s |-> f(s), * |-> n)
data ProbMap a = ProbMap a (IntMap a) deriving (Show, Ord)

getMap :: ProbMap a -> IntMap a
getMap (ProbMap _ m) = m

valueProbMap :: Int -> ProbMap a -> a
valueProbMap k (ProbMap n map1) = IM.findWithDefault n k map1

instance (Fractional a, Ord a) => Eq (ProbMap a) where
  (ProbMap n1 map1) == (ProbMap n2 map2) = n1 == n2 && map1 == map2

instance (Show a, Fractional a, Ord a) => CLat (ProbMap a) where
  leq (ProbMap n1 map1) (ProbMap n2 map2)
    | n1 > n2 = False
    | n2 == 1 = and (IM.mapWithKey (\s v -> IM.findWithDefault n1 s map1 <= v) map2)
    | n1 == 0 = and (IM.mapWithKey (\s v -> v <= IM.findWithDefault n2 s map2) map1)
    | otherwise = error "invalid form in leq"
  top _ = ProbMap 1 IM.empty
  bot _ = ProbMap 0 IM.empty
  meet (ProbMap n1 map1) (ProbMap n2 map2) = ProbMap (min n1 n2) $ IM.unionWith min map1 map2
  join (ProbMap n1 map1) (ProbMap n2 map2) = ProbMap (max n1 n2) $ IM.unionWith max map1 map2

-- return value = [(values mapped to 0, Just (s, v)), ...]
cornerPoints :: ProbMap Rational -> [IntMap Rational]
cornerPoints (ProbMap n ci_map) = map fromCornerToMap $ hCornerPoints (IM.map (const 0) ci_map) 0 Nothing n ci_map

-- cornerPoints' :: IntMap Rational -> ProbMap Rational -> [([Int], Maybe (Int, Rational))]
cornerPoints' :: IntMap Rational -> ProbMap Rational -> [IntMap Rational]
cornerPoints' h_xi1 (ProbMap n ci_map) =
  map fromCornerToMap $ hCornerPoints h_xi1 (sum (IM.filterWithKey (\k _ -> IM.lookup k h_xi1 /= Just 0) ci_map)) Nothing n ci_map

fromCornerToMap :: ([Int], Maybe (Int, Rational)) -> IntMap Rational
fromCornerToMap (ls, Just (s, v)) = IM.insertWith min s v $ IM.fromListWith min $ map (, 0) ls
fromCornerToMap (ls, Nothing) = IM.fromListWith min $ map (, 0) ls


-- pre: sum ci_map >= n
hCornerPoints :: IntMap Rational -> Rational -> Maybe (Int, Rational) -> Rational -> IntMap Rational -> [([Int], Maybe (Int, Rational))]
hCornerPoints f min_value flag n ci_map
  | null ci_map =
    case flag of
      Just (s, v) -> [([], Just (s, n/v)) | 0 < n && n < v && (f!s * v) <= n]
      Nothing     -> [([], Nothing) | n == 0]
  | min_value - maybe (maximumSafe $ IM.filterWithKey (\s _ -> not $ states0 s) ci_map) (const 0) flag > n = []
  | otherwise =
    let ((s, v), ci_map2) = IM.deleteFindMin ci_map in
    let map_0 = if states0 s && (n <= sum ci_map2 + maybe 0 snd flag)
          then map (first (s:)) (hCornerPoints f min_value flag n ci_map2) -- s |-> 0
          else [] in
    let map_01 = if v <= n
          then map_0 ++ hCornerPoints f (min_value-v) flag (n-v) ci_map2 -- s |-> 1
          else map_0 in
    if isJust flag || not (IM.member s f) then map_01 else map_01 ++ hCornerPoints f (if states0 s then min_value else min_value-v*(1-f!s)) (Just (s, v)) n ci_map2
  where
    states0 s = IM.lookup s f == Just 0

maximumSafe :: (Foldable t, Ord a, Num a) => t a -> a
maximumSafe m
  | null m = 0
  | otherwise = maximum m


--
-- For MC
--

type DeltaMC a = Int -> IntMap a

fMC :: (Ord a, Fractional a) => Int -> DeltaMC a -> (Int -> Bool) -> ProbMap a -> ProbMap a
fMC stateNum delta bad prob_map1
  | prob_map1 == ProbMap 0 IM.empty = ProbMap 1 $ IM.fromList $ map (, 0) $ filter (not . bad) [0..stateNum-1]
  | otherwise = ProbMap 1 map2
  where
    map2 = IM.fromList $ filter ((/= 1) . snd) $ map (\s -> (s, g s)) $ filter (not . bad) [0..stateNum-1]
    g s = sum $ IM.mapWithKey (\ns p -> p * valueProbMap ns prob_map1) (delta s)

-- X_i = ProbMap n f = (s |-> f(s), * |-> n) in [0, 1]^S
-- C_i = ProbMap n f = {d: S -> [0, 1] | Sigma_s f(s)*d(s) <= n } in ([0, 1]^S)^down
instance (Show a, Ord a, Fractional a) => CLatPN (ProbMap a) Int where
  type MemoInfo (ProbMap a) Int = ProbMap a
  gamma_leq h prob_map1 n memo =
    if M.member (n+1) memo
      then h_gamma_leq prob_map1 (n+1) memo
      else h_gamma_leq (h prob_map1) n memo
    where
      h_gamma_leq prob_map1 n memo =
        let (ProbMap lambda map2) = memo M.!n in
        sum (IM.mapWithKey (\k v -> valueProbMap k prob_map1 * v) map2) <= lambda


funcSettingMC :: DeltaMC Rational -> (Int -> Bool) -> Heuristics (ProbMap Rational) Int
funcSettingMC delta bad = Heuristics {fCandidate = hCa, fDecide = hDe delta bad, fConflict = hCoMeetBMC}

hCa :: (Eq a, Num a) => ProbMap a -> Problem (ProbMap a) -> Memo (ProbMap a) Int -> IO (Int, Memo (ProbMap a) Int)
hCa _ Problem{safeElem=ProbMap n map} memo  -- {d | d(s0) <= lambda }
  | n == 1 && IM.size map == 1 =
    return (0, if M.member 0 memo
      then memo
      else
        let (s0, lambda) = IM.findMin map in M.singleton 0 (ProbMap lambda $ IM.singleton s0 1))
  | otherwise = error "invalid form"

hDe :: (Eq a, Num a) => DeltaMC a -> (Int -> Bool) -> ProbMap a -> Int -> Problem (ProbMap a) -> Memo (ProbMap a) Int -> IO (Int, Memo (ProbMap a) Int)
hDe delta bad _ ci _ memo
  | M.member (ci+1) memo = return (ci+1, memo)
  | otherwise =
      let (ProbMap n ci_map) = memo M.!ci in
      let (ci_bad, ci_good) = IM.partitionWithKey (\s _ -> bad s) ci_map in
      -- ns |-> Sigma_{s: good} ci_map(s)*delta(s, a_s, ns)
      let nss = concatMap (IM.keys . delta) $ IM.keys ci_good in do
      let ret_map = IM.fromList $ filter ((/= 0) . snd) $ map (\ns -> (ns, sum $ IM.mapWithKey (\s v -> v * IM.findWithDefault 0 ns (delta s)) ci_good)) $ nub nss
      return (ci+1, M.insert (ci+1) (ProbMap (n - sum ci_bad) ret_map) memo)

hCoInitMC :: ProbMap a -> Int -> Problem (ProbMap a) -> Memo (ProbMap a) Int -> IO (ProbMap a, Memo (ProbMap a) Int)
hCoInitMC xi1 _ Problem{b=h} memo = return (h xi1, memo)

hCoMeet01MC :: ProbMap Rational -> Int -> Problem (ProbMap Rational) -> Memo (ProbMap Rational) Int -> IO (ProbMap Rational, Memo (ProbMap Rational) Int)
hCoMeet01MC xi1 ci pb memo = do
  (ret, _) <- hCoMeet01 xi1 (memo M.!ci) pb M.empty
  return (ret, memo)

hCoMeetBMC :: ProbMap Rational -> Int -> Problem (ProbMap Rational) -> Memo (ProbMap Rational) Int -> IO (ProbMap Rational, Memo (ProbMap Rational) Int)
hCoMeetBMC xi1 ci pb memo = do
  (ret, _) <- hCoMeetB xi1 (memo M.!ci) pb M.empty
  return (ret, memo)


--
-- For MDP
--

type Delta a = Int -> [IntMap a]

f :: (Ord a, Fractional a) => Int -> Delta a -> (Int -> Bool) -> ProbMap a -> ProbMap a
f stateNum delta bad prob_map1
  | prob_map1 == ProbMap 0 IM.empty = ProbMap 1 $ IM.fromList $ map (, 0) $ filter (not . bad) [0..stateNum-1]
  | otherwise = ProbMap 1 map2
  where
    map2 = IM.fromList $ filter ((/= 1) . snd) $ map (\s -> (s, g s)) $ filter (not . bad) [0..stateNum-1]
    g s = maximumSafe $ map (sum . IM.mapWithKey (\ns p -> p * valueProbMap ns prob_map1)) (delta s)

f' :: (Ord a, Num a) => [Int] -> Delta a -> ProbMap a -> ProbMap a
f' safes delta prob_map1 = ProbMap 1 map2
  where
    map2 = IM.fromList $ filter ((/= 1) . snd) $ map (\s -> (s, g s)) safes
    g s = maximumSafe $ map (sum . IM.mapWithKey (\ns p -> p * valueProbMap ns prob_map1)) (delta s)

instance (Show a, Ord a, Fractional a) => CLatPN (ProbMap a) (ProbMap a) where
  type MemoInfo (ProbMap a) (ProbMap a) = [ProbMap a]
  gamma_leq h prob_map1 (ProbMap lambda map2) _ = sum (IM.mapWithKey (\k v -> valueProbMap k (h prob_map1) * v) map2) <= lambda

funcSetting :: Delta Rational -> (Int -> Bool) -> Heuristics (ProbMap Rational) (ProbMap Rational)
funcSetting delta bad = Heuristics {fCandidate = hCa, fDecide = hDe, fConflict = hCoMeetB}
  where
    hCa _ Problem{safeElem=ProbMap n map} memo  -- {d | d(s0) <= lambda }
      | n == 1 && IM.size map == 1 =
        let (s0, lambda) = IM.findMin map in return (ProbMap lambda $ IM.singleton s0 1, memo)
      | otherwise = error "invalid form"
    hDe xi1 ci@(ProbMap n ci_map) _ memo =
      case M.lookup ci memo >>= find (\(ProbMap n' ci1_map) -> (sum . IM.mapWithKey (\ns p -> p * valueProbMap ns xi1)) ci1_map > n') of
        Just ci1 -> return (ci1, memo)
        _ -> do
          let (ci_bad, ci_good) = IM.partitionWithKey (\s _ -> bad s) ci_map
          let ci_good' = IM.filterWithKey (\s _ -> not $ null (delta s)) ci_good
          -- s |-> a_s
          let a_map = IM.mapWithKey (\s _ -> fst $ maximumBy (comparing snd) $ zip [0..] $ map (sum . IM.mapWithKey (\ns p -> p * valueProbMap ns xi1)) (delta s)) ci_good'
          -- ns |-> Sigma_{s: good} ci_map(s)*delta(s, a_s, ns)
          let nss = concatMap (\s -> IM.keys $ delta s!!(a_map!s)) $ IM.keys ci_good'
          let ret_map = foldl (\current ns -> IM.insert ns (sum $ IM.mapWithKey (\s v -> v * IM.findWithDefault 0 ns (delta s!!(a_map!s))) ci_good') current) IM.empty $ nub nss
          let ci1 = ProbMap (n - sum ci_bad) $ IM.filter (/= 0) ret_map
          return (ci1, M.insertWith (++) ci [ci1] memo)

hCoMeetB :: ProbMap Rational -> ProbMap Rational -> Problem (ProbMap Rational) -> Memo (ProbMap Rational) (ProbMap Rational) -> IO (ProbMap Rational, Memo (ProbMap Rational) (ProbMap Rational))
hCoMeetB xi1 ci Problem{b=h} memo =
  let corners = cornerPoints' (getMap $ h xi1) ci in
  return (ProbMap 1 $ IM.union (IM.unionsWith min corners) (getMap $ h xi1), memo)

hCoMeet01 :: ProbMap Rational -> ProbMap Rational -> Problem (ProbMap Rational) -> Memo (ProbMap Rational) (ProbMap Rational) -> IO (ProbMap Rational, Memo (ProbMap Rational) (ProbMap Rational))
hCoMeet01 xi1 ci Problem{b=h} memo = do
  let corners = cornerPoints' (getMap $ h xi1) ci
  if null corners then
    return (h xi1, memo)
  else do
    let m = IM.filter (0 ==) $ getMap $ h xi1
    return (ProbMap 1 $ IM.union (IM.unionsWith min corners) m, memo)
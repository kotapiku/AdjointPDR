{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}
module ForMDPS (
  DeltaS (..),
  MyFml (..),
  funcSettingMC,
  funcSettingMCS,
  funcSetting,
  funcSettingS,
  initialN,
  fS,
  fS',
  fMCS,
  hCoMCS,
  hCoS,
  checkFml,
  convertDelta,
  convertDeltaMC,
  convertNtoM,
  isBad,
) where

import           AdjointPDR
import           Control.Monad.Zip  (mzip)
import           Data.IntMap        (IntMap, (!))
import qualified Data.IntMap        as IM
import           Data.List
import qualified Data.Map           as M
import           Data.Maybe         (isNothing)
import           Data.SBV           hiding (And, partition)
import           Data.SBV.Internals (CV, CVal (..), cvVal)
import           ForMDP

--
-- Symbolic
--

type Vars = [(String, (Int, Int), Int)] -- [(var name, (min, max), init)]
type Assigns = [(String, MyFml)] -- (s, fml) expresses (s |-> eval fml)
data DeltaS a = DeltaS {
  vars  :: Vars,
  delta :: [(MyFml, [(a, Assigns)])], -- [(guard, nexts)] where nexts=[(assigns, p)]
  bad   :: MyFml }

data MyFml = And [MyFml]
            | Or [MyFml]
            | Eq MyFml MyFml
            | Leq MyFml MyFml
            | Le MyFml MyFml
            | C Int
            | V String
            | Add [MyFml]
            | Mult [MyFml]
            | Not MyFml
            deriving (Show, Eq)

evalFml :: M.Map String Int -> MyFml -> Int
evalFml m (C r)     = r
evalFml m (V v)     = m M.!v
evalFml m (Add xs)  = sum $ map (evalFml m) xs
evalFml m (Mult xs) = foldl (\current fml -> current * evalFml m fml) 1 xs
evalFml m _         = error "invalid form of delta"

checkFml :: M.Map String Int -> MyFml -> Bool
checkFml m (And xs)        = all (checkFml m) xs
checkFml m (Or xs)         = any (checkFml m) xs
checkFml m (Eq fml1 fml2)  = evalFml m fml1 == evalFml m fml2
checkFml m (Leq fml1 fml2) = evalFml m fml1 <= evalFml m fml2
checkFml m (Le fml1 fml2)  = evalFml m fml1 < evalFml m fml2
checkFml m (Not fml)       = not $ checkFml m fml
checkFml m _               = error "invalid form of delta"

convertNtoM :: Vars -> Int -> M.Map String Int
convertNtoM vars = h (reverse vars)
  where
    h [] n                 = M.empty
    h ((v, (min, max), _):vars) n =
      let len_mm = max - min + 1 in M.insert v (min + (n `mod` len_mm)) $ h vars (n `div` len_mm)

convertMtoN :: Vars -> M.Map String Int -> Int
convertMtoN vars = h (reverse vars)
  where
    h [] m                      = 0
    h ((v, (min, max), _):vs) m = (max - min + 1) * h vs m +  m M.!v - min

stateNum :: Vars -> Int
stateNum = foldl (\current (_, (min, max), _) -> current*(max-min+1)) 1

initialN :: DeltaS a -> Int
initialN DeltaS{vars} = convertMtoN vars $ M.fromListWith (+) $ map h vars
  where
    h (v, (min, max), n) = if n < min || max < n then error "invalid initial value" else (v, n)

isBad :: DeltaS a -> (Int -> Bool)
isBad DeltaS{vars, bad} s = checkFml (convertNtoM vars s) bad

--
-- For MC
--

convertDeltaMC :: Num a => DeltaS a -> DeltaMC a
convertDeltaMC DeltaS{vars, delta} n =
  let m = convertNtoM vars n in
    IM.fromListWith (+) $ maybe [(n, 1)] (map (\(r, nexts) -> (convertMtoN vars (foldl (\current (s, fml) -> M.insert s (evalFml m fml) current) m nexts), r)) . snd) $ find (\(fml, _) -> checkFml m fml) delta

fMCS :: (Num a, Ord a, Fractional a) => DeltaS a -> ProbMap a -> ProbMap a
fMCS sd@DeltaS{vars} = fMC (stateNum vars) (convertDeltaMC sd) (isBad sd)

funcSettingMCS :: DeltaS Rational -> Heuristics (ProbMap Rational) Int
funcSettingMCS sd@DeltaS{vars} = Heuristics {fCandidate = hCa, fDecide = hDe (convertDeltaMC sd) (isBad sd), fConflict = hCoMCS sd}

hCoMCS :: DeltaS Rational -> ProbMap Rational -> Int -> Problem (ProbMap Rational) -> Memo (ProbMap Rational) Int -> IO (ProbMap Rational, Memo (ProbMap Rational) Int)
hCoMCS sd@DeltaS{vars, delta} xi1 yi pb memo = do
  (ret, _) <- hCoS sd xi1 (memo M.!yi) pb M.empty
  return (ret, memo)

--
-- For MDP
--

convertDelta :: Num a => DeltaS a -> Delta a
convertDelta DeltaS{vars, delta} n =
  let m = convertNtoM vars n in
    map (IM.fromListWith (+) . map (\(r, nexts) -> (convertMtoN vars (foldl (\current (s, fml) -> M.insert s (evalFml m fml) current) m nexts), r)) . snd) $ filter (\(fml, _) -> checkFml m fml) delta

fS :: (Num a, Ord a, Fractional a) => DeltaS a -> ProbMap a -> ProbMap a
fS sd@DeltaS{vars} = f (stateNum vars) (convertDelta sd) (isBad sd)

fS' :: (Num a, Ord a, Fractional a) => [Int] -> DeltaS a -> ProbMap a -> ProbMap a
fS' safes sd@DeltaS{vars} = f' safes (convertDelta sd)

funcSettingS :: DeltaS Rational -> Heuristics (ProbMap Rational) (ProbMap Rational)
funcSettingS sd@DeltaS{vars} = funcSetting (convertDelta sd) (isBad sd)

hCoS :: DeltaS Rational -> ProbMap Rational -> ProbMap Rational -> Problem (ProbMap Rational) -> Memo (ProbMap Rational) (ProbMap Rational) -> IO (ProbMap Rational, Memo (ProbMap Rational) (ProbMap Rational))
hCoS sd@DeltaS{vars, delta} xi1 yi Problem{b=h} memo = do
  let corners = cornerPoints' (getMap $ h xi1) yi
  if null corners then
    return (h xi1, memo)
  else do
    let current = IM.unionsWith min corners
    let safe = filter (not . isBad sd) [0..stateNum vars-1]
    let vs = filter (`IM.notMember` current) safe -- solve problem of these variables
    let (zs, d') = mapAccumL (\ls ((fml, nexts), i) -> let (ls1, ls2) = partition (\n -> checkFml (convertNtoM vars n) fml) ls in (ls2, (ls1, nexts, i))) safe $ zip delta [0..]

    let current_xi1 = IM.union current $ IM.fromListWith (+) $ map (\s -> (s, valueProbMap s xi1)) vs
    if leq (h xi1) (ProbMap 1 current_xi1)
      then return (ProbMap 1 current_xi1, memo) -- to get valid earlier
      else do
        result <- satWith z3 (do
          as <- mapM (\i -> sbvExists ("a" ++ show i)) [0..length delta-1]
          bs <- mapM (\i -> sbvExists ("b" ++ show i)) [0..length delta-1]
          mapM_ (\(ls, nexts, i) ->
            case ls `intersect` vs of
              [] -> return ()
              ls' -> do
                let (a, b) = (as!!i, bs!!i)
                mapM_ (\s -> do
                  constrain $ sum (map (calcLeft as bs d' s current) nexts) .<= a*fromIntegral s+b
                  constrain $ fromRational (valueProbMap s $ h xi1) .<= a*fromIntegral s + b
                  ) $ getCheckPoints nexts current sd (map (\(ls, _, i) -> (ls, i)) d') ls'
                constrain $ a*fromIntegral (minimum ls') + b .<= 1
                constrain $ a*fromIntegral (maximum ls') + b .<= 1
              ) d'
          mapM_ (\(s, v) -> case find (\(ls, _, _) -> s `elem` ls) d' of
                          Just (_, nexts, _) -> constrain $ sum (map (calcLeft as bs d' s current) nexts) .<= fromRational v
                          Nothing -> return ()) $ IM.assocs current
          solve [])
        let ret1 = ProbMap 1 (IM.union current $ getMap $ h xi1)
        if modelExists result
          then do
            let m = getModelDictionary result
            let ret2 = ProbMap 1 $ IM.filter (/=1) $ IM.union current $ IM.fromListWith (+) $ map (, 0) zs ++ concatMap (\(ls, _, i) -> map (\n -> (n, max (valueProbMap n xi1) $ fromCVtoRational (m M.!("a" ++ show i))*toRational n+fromCVtoRational (m M.!("b" ++ show i)))) ls) d'
            if leq (h xi1) ret2
              then return (ret2, memo)
              else return (ret1, memo)
          else return (ret1, memo)
  where
    fromCVtoRational :: CV -> Rational
    fromCVtoRational cv = case cvVal cv of
      CRational r -> r
      _           -> error "invalid form"
    calcLeft as bs d' p1 current (p, ns) =
      let m1 = convertNtoM vars p1 in
      let n' = convertMtoN vars $ applyAssigns ns m1 in
        fromRational p*(if IM.member n' current
          then fromRational (current IM.!n')
          else if isBad sd n' then fromRational 1
          else case find (\(ls, _, _) -> n' `elem` ls) d' of
            Just (_, _, i) -> (as!!i)*(fromIntegral n' :: SBV Rational)+(bs!!i)
            Nothing        -> 0)

applyAssigns :: Assigns -> M.Map String Int -> M.Map String Int
applyAssigns ns m = foldl (\current (s, fml) -> M.insert s (evalFml m fml) current) m ns

data PointsWhere = InMap Int | Bad | InAB Int deriving (Eq)
getCheckPoints :: [(a, Assigns)] -> IntMap Rational -> DeltaS Rational -> [([Int], Int)] -> [Int] -> [Int]
getCheckPoints nexts current sd@DeltaS{vars} d = nub . h Nothing
  where
    h :: Maybe (Int, [[PointsWhere]]) -> [Int] -> [Int]
    h _ [] = []
    h old (s:ls') =
      let now = map (\(_, ns) -> let s' = convertMtoN vars $ applyAssigns ns (convertNtoM vars s)
            in if IM.member s' current
              then InMap s'
              else if isBad sd s'
              then Bad
              else InAB $ maybe (length d + 1) snd $ find (\(ls, _) -> s' `elem` ls) d) nexts in
      case old of
        Nothing -> h (Just (s, [now])) ls'
        Just (olds, oldp) ->
          if now `elem` oldp
            then h (Just (s, oldp)) ls'
            else olds:s:h (Just (s, now:oldp)) ls'

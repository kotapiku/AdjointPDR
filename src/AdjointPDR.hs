{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module AdjointPDR (
  adjointPDR,
  CLat (..),
  CLatPN (..),
  Memo,
  Options (..),
  Verbosity (..),
  Heuristics (..),
  Problem (..),
  PDRAnswer (..),
  defaultOpt
) where

import           Control.Monad (when)
import qualified Data.Map      as Map
import           Data.Stack

--
-- AdjointPDR^AI (b, p, gamma: L -> C, \ol{b}, \ol{b}_r)
--

class (Show a) => CLat a where
  leq :: a -> a -> Bool
  bot :: a -> a          -- include dummy argument
  top :: a -> a
  meet :: a -> a -> a
  join :: a -> a -> a

class (CLat p, Show n) => CLatPN p n where -- p = L, n = C
  type MemoInfo p n
  gamma_leq :: (p -> p) -> p -> n -> Memo p n -> Bool -- gamma (b p) <=^? n

type PosiChain p = [p]         -- [x_{n-1}, ..., x_0=bot]
type NegSeq n = Stack n -- Stack (n-i) [y_i, ..., y_{n-1}]
newtype PDRConfig p n = PN (PosiChain p, NegSeq n) deriving (Show)
data PDRAnswer p n = Valid (PosiChain p) | InValid (NegSeq n) deriving (Show)

type Memo p n = Map.Map n (MemoInfo p n)
data Problem p = Problem {
  b        :: p -> p,
  safeElem :: p
}
data (CLatPN p n) => Heuristics p n = Heuristics {
  fCandidate :: p -> Problem p -> Memo p n -> IO (n, Memo p n),
  fDecide    :: p -> n -> Problem p -> Memo p n -> IO (n, Memo p n),
  fConflict  :: p -> n -> Problem p -> Memo p n -> IO (p, Memo p n) }

-- options
data Options = Options { optPrint :: Verbosity, optMaxStep :: Maybe Int }
data Verbosity = PrintAll | PrintRule | PrintLength | NoPrint
defaultOpt :: Options
defaultOpt = Options { optPrint = NoPrint, optMaxStep = Nothing }

printRuleWith :: (CLatPN p n) => Options -> String -> PDRConfig p n -> IO ()
printRuleWith opts rule (PN (posi_chain, neg_seq)) =
  case optPrint opts of
    PrintAll -> do
      putStrLn $ rule ++ replicate (11 - length rule) ' ' ++ show posi_chain
      putStrLn $ replicate 11 ' ' ++ show neg_seq
    PrintRule -> putStrLn rule
    PrintLength ->
      when (rule == "Init:" || rule == "Unfold:") $ putStrLn $ "length of posi_chain: " ++ show (length posi_chain)
    _ -> return ()

-- check whether mu b <= p in L
-- assume gamma's order-embeddingness and forward completeness
adjointPDR :: forall p n. CLatPN p n => Options -> Heuristics p n -> Problem p -> IO (PDRAnswer p n)
adjointPDR opts func_s pb@Problem{b, safeElem=p} =
  let initSeq = PN ([top p, bot p], stackNew) in
    printRuleWith opts "Init:" initSeq >> loop (optMaxStep opts) Map.empty initSeq
  where
    loop :: Maybe Int -> Memo p n -> PDRConfig p n -> IO (PDRAnswer p n)
    loop (Just 0) _ _ = error "terminate due to the limitation of steps"
    loop n memo (PN (xs, ys)) =
      case stackPop ys of
        Nothing
          | or [leq (xs !! i) (xs !! (i+1)) | i <- [0..(length xs - 2)]]
            -> return $ Valid xs
          | leq (head xs) p -- x_{n-1} <=^? p
            -> let next = PN (top p:xs, stackNew) in
               printRuleWith opts "Unfold:" next >> loop (fmap (\m -> m - 1) n) memo next
          | otherwise -> do
              (x, memo') <- fCandidate func_s (head xs) pb memo
              let next = PN (xs, stackPush ys x)
              printRuleWith opts "Candidate:" next >> loop (fmap (\m -> m - 1) n) memo' next
        Just (ys', yi) ->
          let sizeOfys = fromIntegral $ toInteger $ stackSize ys in
          let xi1 = xs !! sizeOfys in do
            if length xs == sizeOfys then return $ InValid ys
            else do
              if gamma_leq b xi1 yi memo
                then do
                  (x, memo') <- fConflict func_s xi1 yi pb memo
                  let xs' = zipWith (\x' i -> if i == length xs - 1 || i < sizeOfys - 1 then x' else meet x x') xs [0..]
                  let next = PN (xs', ys')
                  printRuleWith opts "Conflict:" next >> loop (fmap (\m -> m - 1) n) memo' next
                else do
                  (x, memo') <- fDecide func_s xi1 yi pb memo
                  let next = PN (xs, stackPush ys x)
                  printRuleWith opts "Decide:" next >> loop (fmap (\m -> m - 1) n) memo' next

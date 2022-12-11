{-# language BangPatterns #-}
module Y2021.Day3 (solve) where

import Data.List
import Debug.Trace

-- first arg is Part
solve :: Int -> String -> IO ()
solve n = print . compute n . parse

compute :: Int -> [Rec] -> Int
compute 1 (r:rs) = gamma * epsilon
  where
  (S sums len) = foldl' f (S r 1) rs
  winners = map (\s -> if s > len `div` 2 then 1 else 0) sums
  loosers = map (\w -> (w + 1) `mod` 2) winners
  gamma = readBin winners
  epsilon = readBin loosers
  f :: St -> Rec -> St
  f (S sums len) word = S (merge sums word) (len+1)
  merge :: Sums -> Rec -> Sums
  merge = zipWith (+)

compute 2 rs = oxy * co2
  where
  tree = foldl' insertT emptyT rs
  oxyB = locate oxyP tree
  oxy = readBin oxyB
  co2B = locate co2P tree
  co2 = readBin co2B

type Sums = [Int]
data St = S !Sums !Int
type Rec = [Int]

parse :: String -> [Rec]
parse = map readRec . lines

type Child = (Int, T)
data T = N [Child]

emptyC = (0, undefined)
isEmptyC (0, _) = True
isEmptyC _ = False
emptyT = N [emptyC, emptyC]
isEmptyT :: T -> Bool
isEmptyT (N cs) = all isEmptyC cs
subtr (_, t) = t

insertT :: T -> Rec -> T
insertT _ []     = N [emptyC, emptyC]
insertT t@(N [c0@(n0, t0), c1@(n1, t1)]) (x:xs)
  = N [c0', c1']
  where
  t0' = if isEmptyC c0 then emptyT else t0
  t1' = if isEmptyC c1 then emptyT else t1
  (c0', c1') = case x of
    0 -> ((n0+1, insertT t0' xs), c1)
    1 -> (c0, (n1+1, insertT t1' xs))
    _ -> error "unexpected bit"

locate :: ([Child] -> Int) -> T -> Rec
locate p = reverse . go []
  where
  go bs t@(N cs)
    | isEmptyT t = bs
    | isEmptyC (cs !! 0) = go (1:bs) (subtr $ cs !! 1)
    | isEmptyC (cs !! 1) = go (0:bs) (subtr $ cs !! 0)
    | otherwise = go (b:bs) (subtr c)
      where
      b = p cs
      c = cs !! b

oxyP [(n0,_), (n1,_)]
  | n0 > n1   = 0
  | otherwise = 1

co2P [(n0,_), (n1,_)]
  | n0 > n1   = 1
  | otherwise = 0

-- Debug
showT :: T -> String
showT t =
  "Tree: fromList:" ++ go t []
  where
  go (N [c0, c1]) bs =
    if isEmptyC c0 && isEmptyC c1
    then '\n' : reverse bs
    else
      s0 ++ s1
    where
    s0 = if isEmptyC c0 then "" else go (subtr c0) ('0':bs)
    s1 = if isEmptyC c1 then "" else go (subtr c1) ('1':bs)

instance Show T where
  show (N cs) = "(N" ++ (concatMap s cs) ++ ")"
    where
    s (n,t) = if n == 0 then "(0, _)" else
      "(" ++ show n ++ "," ++ show t ++ ")"


-- Aux
readRec = map digit
  where
  digit '0' = 0
  digit '1' = 1

readBin :: Rec -> Int
readBin = foldl' (\s x -> s*2 + x) 0

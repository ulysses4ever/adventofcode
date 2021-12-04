{-# language BangPatterns #-}
{-# language ViewPatterns   #-}
module Y2021.Day3 (solve) where

import Data.List
import Debug.Trace
import GHC.Stack

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

compute 2 rs = trace (showT tree) 0
  where
  !tree = foldl' insertT emptyT rs

readBin :: [Int] -> Int
readBin = foldl' (\s x -> s*2 + x) 0

type Sums = [Int]
data St = S !Sums !Int
type Rec = [Int]

parse :: String -> [Rec]
parse = map readRec . lines

type Child = (Int, T)
data T = N [Child]
       deriving Show

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

locate :: (Child -> Child -> Int) -> T -> Rec
locate p = undefined

{-
N (1,N (1,N emptyC (1,N emptyC (1,N emptyC emptyC))) emptyC) (1,N (1,N (1,N (1,N emptyC emptyC) emptyC) emptyC) emptyC)

-}
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


readRec = map digit
  where
  digit '0' = 0
  digit '1' = 1

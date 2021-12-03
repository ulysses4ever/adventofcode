{-# language BangPatterns #-}
{-# language ViewPatterns   #-}
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
data T = T Child Child

emptyC = (0, undefined)
isEmptyC (0, _) = True
isEmptyC _ = False
emptyT = T emptyC emptyC
isEmptyT :: T -> Bool
isEmptyT (T c0 c1) = isEmptyC c0 && isEmptyC c1

insertT :: T -> Rec -> T
insertT _ []     = T emptyC emptyC
insertT (T c0@(n0, t0) c1@(n1, t1)) (x:xs) = T c0' c1'
  where
  (c0', c1') = case x of
    0 -> ((n0+1, insertT t0 xs), c1)
    1 -> (c0, (n1+1, insertT t1 xs))
    _ -> error "unexpected bit"

showT :: T -> String
showT (T c0 c1) =
  "Tree: fromList:\n" ++ go c0 "0" ++ go c1 "1"
  where
  go c@(_, t) bs
    | isEmptyC c = reverse $ tail bs
    | otherwise  =
      case t of
        (T c0 c1) -> go c0 ('0':bs) ++ "\n" ++ go c1 ('1':bs)


readRec = map digit
  where
  digit '0' = 0
  digit '1' = 1

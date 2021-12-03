{-# language BangPatterns #-}
{-# language LambdaCase   #-}
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

compute 2 rs = 0

readBin :: [Int] -> Int
readBin = foldl' (\s x -> s*2 + x) 0

type Sums = [Int]
data St = S !Sums !Int
type Rec = [Int]

parse :: String -> [Rec]
parse = map readRec . lines

readRec = map digit
  where
  digit '0' = 0
  digit '1' = 1

#!/usr/bin/env cabal
{- cabal:
build-depends: base
-}
{-# language TypeApplications, StrictData, RecordWildCards #-}
{-# OPTIONS_GHC -Wall -O1 #-}
module Main where

import Data.List

len :: Int
len = 25

-- Part 1

res1 :: [Integer] -> Integer
res1 xs = firstNotCheck cs ts
  where
  all_tails = tails xs
  ts = take len all_tails
  cs = all_tails !! len

-- If the head of the first list can be represented as a sum
-- of two heads from the second list (of lists),
-- then recur to tails, otherwise return that head.
firstNotCheck :: [Integer] -> [[Integer]] -> Integer
firstNotCheck []     _  = error "all checks"
firstNotCheck (c:cs) ts
  | not $ checkSumsTo c (map head ts) = c
  | otherwise = firstNotCheck cs (map tail ts)

-- If y can be a sum of two from xs
checkSumsTo :: Integer -> [Integer] -> Bool
checkSumsTo y xs = not $ null a
  where
  xs' = filter (< y) xs
  a = [ (t,t') |
    ts  <- tails xs',
    not $ null ts,
    let t : ts' = ts,
    t' <- ts',
    t + t' == y ]

-- Part 2

data Rec = R {
  su :: Integer,
  ma :: Integer,
  mi :: Integer }

-- Takes:
--   all contiguous sums of len n-1 (ss) with metadata (Rec),
--   initial list without first (n-1) elements (xs);
-- Returns:
--   all contiguous sums of len n
sumContN :: [Rec] -> [Integer] -> [Rec]
sumContN ss xs = zipWith f (init ss) xs
  where
  f R{..} x = R (su + x) (x `max` ma) (x `min` mi)

res2 :: Integer -> [Integer] -> Integer
res2 q xs = go (map initRec xs) (tail $ tails xs)
  where
  initRec n = R n n n

  -- Apply sumContN until find q in ss
  go :: [Rec] -> [[Integer]] -> Integer
  go _  []       = error "part 2: not found"
  go ss (ts:tss) = let
    ss' = sumContN ss ts
    in case find ((== q) . su) ss' of
      Just R{..} -> ma + mi
      Nothing -> go ss' tss

getInput :: IO [Integer]
getInput = map (read @Integer) . filter (not . null) . lines <$>
  readFile "input/day-9.txt"

main :: IO ()
main =
  print =<< (\xs -> res2 (res1 xs) xs) <$> getInput

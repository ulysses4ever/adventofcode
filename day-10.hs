#!/usr/bin/env cabal
{- cabal:
build-depends: base, extra
-}
{-# language TypeApplications, RecordWildCards #-}
{-# OPTIONS_GHC -Wall -O1 #-}
module Main where

import Data.List
import Data.List.Extra

-- Part 1

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

diffs :: [Int] -> [Int]
diffs xs = zipWith (-) (tail xs') xs'
  where
  xs' = 0 : sort xs
  --    ^- for the outlet

onesTimesThrees :: [Int] -> Int
onesTimesThrees ds = ones * threes
  where
  ones = count 1 ds
  threes = count 3 ds + 1
  -- for your device -^

-- Part 2

countSeqs :: [Int] -> Integer
countSeqs ds = product . map toInteger . filter (/= 0) $ ys
  where
  onesSeqs = wordsBy (== 3) $ ds
  ys = map (f . length) onesSeqs

-- I don't understand how it works,
-- just figured it from examples
f :: Int -> Int
f 1 = 0
f 2 = 2
f 3 = 4
f n = 2 * f (n - 1) - 1

-- Main

getInput :: IO [Int]
getInput = map (read @Int) . filter (not . null) . lines <$>
  readFile "input/day-10.txt"

main :: IO ()
main = do
  inp <- getInput
  let ds = diffs inp
      res1 = onesTimesThrees ds
      res2 = countSeqs ds
  print ds
  print res1
  print res2

------------------------------------------
--
--   Tests
--
------------------------------------------
test :: [Int]
test = [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]
test_diffs :: [Int]
test_diffs = [1,3,1,1,1,3,1,1,3,1,3]
-- excludes the last 3 for your device

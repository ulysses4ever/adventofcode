#!/usr/bin/env cabal
{- cabal:
build-depends: base, unordered-containers
-}
{-# OPTIONS_GHC -Wall -O2 #-}
module Main where

import Control.Arrow ((&&&))
import Data.List

type P = (Int, Int, Int, Int)
type S = [P]

counts :: Ord a => [a] -> [(a, Int)]
counts = map (head &&& length) . group . sort

nhood :: [Int]
nhood = [-1..1]

neighbours :: P -> [P]
neighbours pt@(x,y,z,w) = [ pt' |
  dx <- nhood,
  dy <- nhood,
  dz <- nhood,
  dw <- nhood,
  let pt' = (x + dx, y + dy, z + dz, w + dw),
  pt' /= pt]

inpToSet :: [String] -> S
inpToSet inp = [ (i,j,0,0) |
  (i, row) <- zip [0..] inp,
  (j, elm) <- zip [0..] row,
  elm == '#']

step :: S -> S
step alive = [pt |
  (pt, n) <- counts $ neighbours =<< alive,
  n == 3 || (n == 2 && pt `elem` alive)]

-- Main

getInput :: IO [String]
getInput =
  lines <$> readFile "input/day-17.txt"

main :: IO ()
main = do
  steps <- iterate step . inpToSet <$> getInput
  let res = length $ steps !! 6
  print res


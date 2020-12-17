#!/usr/bin/env cabal
{- cabal:
build-depends: base, unordered-containers
-}
{-# language OverloadedLists #-}
{-# OPTIONS_GHC -Wall -O1 #-}
module Main where

import Control.Arrow ((&&&))
import Data.List
import qualified Data.HashSet as S

type P = (Int, Int, Int)
type S = S.HashSet P

counts :: Ord a => [a] -> [(a, Int)]
counts = map (head &&& length) . group . sort

nhood :: [Int]
nhood = [-1..1]

neighbours :: P -> [P]
neighbours pt@(x,y,z) = [ pt' |
  dx <- nhood,
  dy <- nhood,
  dz <- nhood,
  let pt' = (x + dx, y + dy, z + dz),
  pt' /= pt]

inpToSet :: [String] -> S
inpToSet inp = S.fromList [ (i,j,0) |
  (i, row) <- zip [0..] inp,
  (j, elm) <- zip [0..] row,
  elm == '#']

step :: S -> S
step alive = S.fromList [pt |
  (pt, n) <- counts $ neighbours =<< S.toList alive,
  n == 3 || (n == 2 && pt `S.member` alive)]

-- Main

getInput :: IO [String]
getInput =
  lines <$> readFile "input/day-17.txt"

main :: IO ()
main = do
  steps <- iterate step . inpToSet <$> getInput
  let res = S.size $ steps !! 6
  print res


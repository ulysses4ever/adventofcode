#!/usr/bin/env cabal
{- cabal:
build-depends: base, unordered-containers
-}
{-# OPTIONS_GHC -Wall -O2 #-}
module Main where

import Data.List
import Data.Bool
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M

type P = (Int, Int, Int, Int)
type S = S.HashSet P
type M = M.HashMap P Int

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
inpToSet inp = S.fromList [ (i,j,0,0) |
  (i, row) <- zip [0..] inp,
  (j, elm) <- zip [0..] row,
  elm == '#']

step :: S -> S
step alive = alive'
  where
  ins m p = M.insertWith (+) p 1 m
  cs = S.foldl' (\m a -> foldl' ins m $ neighbours a) M.empty alive :: M
  alive' = M.foldrWithKey'
    (\pt n s -> bool s (pt `S.insert` s) (n == 3 || (n == 2 && pt `S.member` alive)))
    S.empty 
    cs

-- Main

getInput :: IO [String]
getInput =
  lines <$> readFile "input/day-17.txt"

main :: IO ()
main = do
  steps <- iterate step . inpToSet <$> getInput
  let res = S.size $ steps !! 6
  print res


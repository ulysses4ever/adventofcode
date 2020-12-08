#!/usr/bin/env cabal
{- cabal:
build-depends: base, extra, containers
-}
{-# OPTIONS_GHC -Wall -O1 #-}
module Main where

import Data.List.Extra
import qualified Data.Set as S

type Str = String
type Input = Str
type Group = Str
type Votes1 = Str
type Votes2 = S.Set Char
type Score = Int

groups :: Input -> [Group]
groups = splitOn "\n\n"

-- Part 1
groupToVotes1 :: Group -> Votes1
groupToVotes1 = nubOrd . (filter (/= '\n'))

-- Part 2
groupToVotes2 :: Group -> Votes2
groupToVotes2 = foldr1 S.intersection . map S.fromList . lines

res :: (Group -> votes) -> (votes -> Score) -> Input -> Score
res groupToVotes score =
  sum . map (score . groupToVotes) . groups

main :: IO ()
main = do
  inp <- readFile "input/day-6.txt"
  let
    res1 = res groupToVotes1 length inp
    res2 = res groupToVotes2 S.size inp
  print res1
  print res2


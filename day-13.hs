#!/usr/bin/env cabal
{- cabal:
build-depends: base, extra, numeric-prelude
-}
{-# language TypeApplications #-}
{-# OPTIONS_GHC -Wall -O1 #-}
module Main where

import Control.Arrow
import Data.List.Extra
import Algebra.PrincipalIdealDomain

-- Part 1

part1 :: [String] -> Int
part1 (e : ws : [])  = i * (d - departEarliest)
  where
  departEarliest = read @Int e

  -- IDs
  xs = map (read @Int) .
    filter (/="x") .
    wordsBy (==',') $ ws

  xs' = zip xs $ -- [ (ID, earliest dep. after departEarliest) ]
    map (\x -> (* x) . (+ 1) . (div departEarliest) $ x) xs

  (i, d) = minimumOn snd xs'

part1 _ = error "invalid input"

-- Part 2

part2 :: [String] -> Integer
part2 (_ : ws : [])  = n - b
  where
  Just (n,b) = chineseRemainderMulti . part2' $ ws

part2 _ = error "invalid input"

part2' :: String -> [(Integer, Integer)]
part2' =
    map (\(x,y) -> (y,x)) .
    map (second $ read @Integer) .
    filter ((/="x") . snd) .
    zip [0::Integer ..] .
    wordsBy (==',')

-- Main

getInput :: IO [String]
getInput =
  lines <$> readFile "input/day-13.txt"

main :: IO ()
main = do
  inp <- getInput
  let res1 = part1 inp
  print res1
  let res2 = part2 inp
  print res2
  print "--"

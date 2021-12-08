{-# language BangPatterns #-}
module Y2021.DayN (solve) where

import Aux
import Data.List
import Debug.Trace

-- first arg is Part #
solve :: Int -> String -> IO ()
solve n = print . compute n . parse

type Rec = [Int]
data St = S

compute :: Int -> [Rec] -> Int
compute 1 rs = res
  where
  res = 0

compute 2 rs = res
  where
  res = 0

parse :: String -> [Rec]
parse = map readRec . lines

readRec :: String -> Rec
readRec = undefined

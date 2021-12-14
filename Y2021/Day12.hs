{-# language BangPatterns #-}
module Y2021.Day12 (solve) where

import Aux
import Data.List
import Debug.Trace

-- first arg is Part #
solve :: Int -> String -> IO ()
solve n = print . compute n . parse

type V = String
type Rec = (V,V)
type St = [Rec]

compute :: Int -> [Rec] -> Int
compute 1 g = traceShow g res
  where
    go :: [V] -> V -> Int
    go visited v
      | v == "end" = 1
      | otherwise = sum $ map (go (v:visited)) as
      where
        as = adj v g \\ visited
    res = go [] "start"

compute 2 rs = res
  where
  res = 0

adj :: V -> St -> [V]
adj v = concatMap pick
  where
    pick (v1,v2)
      | v == v1 = [v2]
      | v == v2 = [v1]
      | otherwise = []

parse :: String -> [Rec]
parse = map readRec . lines

readRec :: String -> Rec
readRec s = (v1,v2)
  where
    [v1,v2] = split (=='-') s

{-# language BangPatterns #-}
module Y2021.Day12 (solve) where

import Aux
import Data.List
import Data.Char
import Debug.Trace

-- first arg is Part #
solve :: Int -> String -> IO ()
solve n = print . compute n . parse

type V = String
type Rec = (V,V)
type St = [Rec]

compute :: Int -> [Rec] -> Int
compute 1 g = trace (intercalate "\n" res) (length res)
  where
    go :: [V] -> [V] -> V -> [String]
    go visited small v
      | v == "end" = [intercalate "," $ reverse (v:visited)]
      | otherwise = concatMap (go (v:visited) small') as
      where
        as = adj v g \\ small
        small'
          | isSmall v = v:small
          | otherwise = small
    res = go [] [] "start"

compute 2 rs = res
  where
  res = 0

compute _ rs = error "unknown part"

isSmall :: V -> Bool
isSmall = all isLower

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

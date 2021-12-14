{-# language BangPatterns #-}
module Y2021.Day12 (solve) where

import Aux
import Data.List
import Data.List.Extra (nubOrd)
import Data.Char
import Debug.Trace

-- first arg is Part #
solve :: Int -> String -> IO ()
solve n = print . compute n . parse

type V = String
type Rec = (V,V)
type St = [Rec]
data St' = S [V] Bool

compute :: Int -> [Rec] -> Int
compute n g = trace (intercalate "\n" res) (length res)
  where
    go :: [V] -> St' -> V -> [String]
    go visited (S small s) v
      | v == "end" = [intercalate "," $ reverse (v:visited)]
      | v == "start" && not (null visited) = []
      | otherwise = concat (go (v:visited) <$> ss <*> as)
      where
        as = adj v g \\ small
        ss
          | not (isSmall v) = [S small s]
          | otherwise = sts
        sts
          | not s && n == 2 = [st', st'']
          | otherwise = [st'']
        st' = S small True
        st'' = S (v:small) s
    res = nubOrd $ go [] (S [] False) "start"

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

{-# language BangPatterns #-}
module Y2021.Day8 (solve) where

import Aux
import Data.List
import Data.Maybe
import Debug.Trace
import Control.Arrow (second)
import Data.List.Extra (groupSortOn)

-- first arg is Part #
solve :: Int -> String -> IO ()
solve n = print . compute n . parse

data Rec = R {
  left :: [String],
  right :: [String] }
  deriving Show
type St = [String]

interestingLengths :: [Int]
--                    1 7 4 8
interestingLengths = [2,3,4,7]

compute :: Int -> [Rec] -> Int
compute 1 rs = res
  where
  countInteresting :: [String] -> Int
  countInteresting = length . filter ((`elem` interestingLengths ) . length)
  res = sum $ map (countInteresting . right) rs

compute 2 rs = res
  where
  rd (R l r) = read $ concatMap (show . decode (sortWords l)) r
  res = sum $ map rd rs

decode :: St -> String -> Int
decode s w = fromJust $ elemIndex w s

sortWords :: [String] -> [String]
sortWords ws = res
  where
  [[n1], [n7], [n4], l5, l6, [n8]] = groupSortOn length ws
  [n2, n3, n5] = sort5 l5 n4 n1
  [n0, n6, n9] = sort6 l6 n4 n1
  res = [n0, n1, n2, n3, n4, n5, n6, n7, n8, n9]

sort6 :: [String] -> String -> String -> [String]
sort6 ws@[u, v, w] n4 n1 = [n0, n6, n9]
  where
  Just n9 = find (n4 `subset`) ws
  ws'@[u', v'] = ws \\ [n9]
  Just n0 = find (n1 `subset`) ws'
  [n6] = ws' \\ [n0]

sort6 _ _ _ = error "not implemented"

sort5 :: [String] -> String -> String -> [String]
sort5 ws@[u, v, w] n4 n1  = [n2, n3, n5]
  where
  Just n3 = find (n1 `subset`) ws
  ws'@[u', v'] = ws \\ [n3]
  (n2, n5) = if length (n4 \\ u') == 2 then (u', v') else (v', u')

sort5 _ _ _ = error "not implemented"

subset s1 s2 = (== length s1) . length $ s1 `intersect` s2

parse :: String -> [Rec]
parse = map readRec . lines

readRec :: String -> Rec
readRec = uncurry R . second tail . break (== "|") . map sort . words

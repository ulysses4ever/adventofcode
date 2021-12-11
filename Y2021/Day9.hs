{-# language BangPatterns #-}
{-# language GADTs        #-}
module Y2021.Day9 (solve) where

import Aux
import Data.List
import GHC.Exts (sortWith)
import Data.Ord
import qualified Data.Set as S
import Control.Arrow (first)
import Debug.Trace
import Foreign.C.Types
import Numeric.LinearAlgebra (Matrix, fromLists, atIndex, cols, rows)

-- first arg is Part #
solve :: Int -> String -> IO ()
solve n = print . compute n . parse

type M = Matrix CInt
type Rec = CInt
type Pt = (Int,Int)
type S = S.Set Pt

compute :: Int -> M -> Int
compute n m = if n == 1 then fromIntegral res1 else res2 -- traceShow m
  where
  f pt (n, mins) = first (n +) $ checkMin m pt mins
  (res1, mins) = foldr f (0,[]) [(r,c) | r <- [1..rows m - 1], c <- [1..cols m - 1]]

  vis s p = any (p `S.member`) s
  f' s p = if vis s p then s else dfs m p S.empty : s
  res2 = product . take 3 . sortWith Down . map S.size $ foldl' f' [] mins

dfs g cur vis =
  foldl'
    (\vis nxt -> if nxt `S.member` vis then vis else dfs g nxt vis)
    (cur `S.insert` vis)
    (open g cur)

open :: M -> Pt -> [Pt]
open m pt = filter ((/= bord). (m `atIndex`)) (nhood pt)

-- 4-neighbourhood of a 2D integer point
nhood :: Pt -> [Pt]
nhood (r,c) = [(r+dr,c+dc) |
  dr <- [-1..1], dc <- [-1..1],
  dr /= 0 || dc /= 0,
  dr == 0 || dc == 0]

-- checks for local minimum, and records if success
checkMin m pt mins
  | and [m `atIndex` pt < m `atIndex` pt' | pt' <- nhood pt]
    = (1 + m `atIndex` pt, pt:mins)
  | otherwise
    = (0,mins)

bord :: Rec
bord = 9

parse :: String -> M
parse s = fromLists m'
  where
    m = map (map readRec) . lines $ s
    cols = length $ head m
    bound = take (cols + 2) $ repeat bord
    addBounds xs = bord : xs ++ [bord]
    m' = bound : (map addBounds m) ++ [bound]

readRec :: Char -> Rec
readRec = read . pure

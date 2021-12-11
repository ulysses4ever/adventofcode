{-# language BangPatterns #-}
{-# language GADTs        #-}
module Y2021.Day9 (solve) where

import Aux
import Data.List
import Debug.Trace
import Foreign.C.Types
import Numeric.LinearAlgebra (Matrix, fromLists, atIndex, cols, rows)

-- first arg is Part #
solve :: Int -> String -> IO ()
solve n = print . compute n . parse

type M = Matrix CInt
type Rec = CInt
data St = S

compute :: Int -> M -> CInt
compute 1 m = res -- traceShow m
  where
  f (r,c) n = n + checkMin m r c
  res = foldr f 0 [(r,c) | r <- [1..rows m - 1], c <- [1..cols m - 1]]

compute 2 rs = res
  where
  res = 0

checkMin m r c |
  m `atIndex` (r,c) < m `atIndex` (r-1,c) &&
  m `atIndex` (r,c) < m `atIndex` (r,c-1) &&
  m `atIndex` (r,c) < m `atIndex` (r+1,c) &&
  m `atIndex` (r,c) < m `atIndex` (r,c+1)  = 1 + m `atIndex` (r,c)
  | otherwise = 0

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

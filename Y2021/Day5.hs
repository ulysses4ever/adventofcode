{-# language BangPatterns  #-}
{-# language TupleSections #-}
module Y2021.Day5 (solve) where

import Aux

import Data.List
import Data.Function
import Data.Foldable
import Data.Traversable
import Control.Monad
import Debug.Trace

import Data.List.Extra (split, chunksOf)
import Data.Tuple.Extra (dupe)

-- first arg is Part #
solve :: Int -> String -> IO ()
solve n = print . compute n . parse

type Pt = (Int,Int)
type Line = (Pt,Pt)
type Rec = Line

compute :: Int -> [Rec] -> Int
compute n rs
  = rs
  & concatMap (fillLine n)
  & sort
  & group
  & filter ((> 1) . length)
  & length

fillLine :: Int -> Line -> [Pt]
fillLine n (p1@(x1,y1), p2@(x2,y2)) = let
  dx = sign $ x2 - x1
  dy = sign $ y2 - y1
  xs = [x1, x1+dx..]
  ys = [y1, y1+dy..]
  in
    if n == 1 && dy /= 0 && dx /= 0
      then []
      else p2 : (takeWhile (/= p2) $ zip xs ys)

parse :: String -> [Rec]
parse = map readRec . lines

readRec :: String -> Rec
readRec input = ((x1,y1),(x2,y2))
  where
  (p1 : arrow : p2 : _) = words input
  [x1, y1, x2, y2] = map read $ concatMap (split (==',')) [p1,p2]

{-# language BangPatterns  #-}
{-# language UnboxedTuples  #-}
{-# language TupleSections #-}
module Y2021.Day5 (solve) where

import Aux

import Data.List
import Data.Function
import Data.Foldable
import Data.Traversable
import Control.Monad
import Debug.Trace

import Data.List.Extra (chunksOf)
import Data.Tuple.Extra (dupe)

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Algorithms.Intro as V (sort)
import Data.Vector.Unboxed (Vector)

-- first arg is Part #
solve :: Int -> String -> IO ()
solve n = print . compute n . parse

type Pt = (Int,Int)
type Line = (Int,Int,Int,Int)

compute :: Int -> Vector Line -> Int
compute n rs
  = rs
  & V.concatMap (fillLine n)
  & V.modify V.sort
  & V.toList -- vector doesn't have group :'-(
  & group    -- cf. https://github.com/haskell/vector/issues/192
  & filter ((> 1) . length)
  & length

fillLine :: Int -> Line -> Vector Pt
fillLine n (x1,y1,x2,y2) = let
  dx = sign $ x2 - x1
  dy = sign $ y2 - y1
  xs = [x1, x1+dx..]
  ys = [y1, y1+dy..]
  p2 = (x2,y2)
  in
    if n == 1 && dy /= 0 && dx /= 0
      then V.empty
      else V.fromList $ p2 : (takeWhile (/= p2) $ zip xs ys)

parse :: String -> Vector Line
parse = V.fromList . map readRec . lines

readRec :: String -> Line
readRec input = (x1,y1,x2,y2)
  where
  (p1 : arrow : p2 : _) = words input
  [x1,y1,x2,y2] = map read $ concatMap (split (==',')) [p1,p2]

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

import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict (HashMap)

type Map = HashMap

-- first arg is Part #
solve :: Int -> String -> IO ()
solve n = print . compute n . parse

type Pt = (Int,Int)
type Line = (Pt,Pt)
type Rec = Line
type St = Map Pt Int

compute :: Int -> [Rec] -> Int
compute n rs = res
  where
  finalState = foldl' (updateState n) M.empty rs
  cnt s x = if x > 1 then s + 1 else s
  res = M.foldl' cnt 0 finalState

updateState :: Int -> St -> Line -> St
updateState n m l = -- trace (showM m) $
  foldl' upd m $ fillLine n l
  where
  upd m p = M.alter ins p m
  ins Nothing = Just 1
  ins (Just n) = Just (n+1)

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

-- Debug, only for the sample
showM :: St -> String
showM m = res
  where
  ps = liftM2 (,) [0..maxCol] [0..maxRow]
  maxCol = 9 -- maximum $ map (fst . fst) ps
  maxRow = 9 -- maximum $ map (snd . fst) ps
  res = unlines $ chunksOf (maxCol + 1) $
    flip map ps $ \(c,r) ->
        case M.lookup (r,c) m of
          Nothing -> '.'
          Just n  -> head $ show n

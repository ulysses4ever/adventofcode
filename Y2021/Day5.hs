{-# language BangPatterns  #-}
{-# language TupleSections #-}
module Y2021.Day5 (solve) where

import Data.List
import Data.Function
import Data.Foldable
import Data.Traversable
import Control.Monad
import Debug.Trace

import Data.List.Extra (split, chunksOf)
import Data.Tuple.Extra (dupe)

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)

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
fillLine n (p1@(x1,y1), p2@(x2,y2))
  | x1 == x2 = (x1,) `map` fillUp y1 y2
  | y1 == y2 = (,y1) `map` fillUp x1 x2
  | n == 2 && (abs (x1 - y1) == abs (x2 - y2) ||
               abs (x1 - x2) == abs (y1 - y2))
      = fillDg (sort [p1, p2])
  | otherwise = []

-- assert: x1 < x2
fillDg :: [Pt] -> [Pt]
fillDg [(x1,y1), (x2,y2)] = zip [x1..x2] ys
  where
  ys = if y1 < y2 then fillUp y1 y2 else fillDw y2 y1

fillUp :: Int -> Int -> [Int]
fillUp x y
  | x < y = [x..y]
  | otherwise = [y..x]

fillDw :: Int -> Int -> [Int]
fillDw x y
  | x < y = [y,y-1..x]
  | otherwise = [x,x-1..y]

parse :: String -> [Rec]
parse = map readRec . lines

readRec :: String -> Rec
readRec input = ((x1,y1),(x2,y2))
  where
  (p1 : arrow : p2 : _) = words input
  [x1, y1, x2, y2] = map read $ concatMap (split (==',')) [p1,p2]

-- Debug
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

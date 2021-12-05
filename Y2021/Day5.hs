{-# language BangPatterns #-}
module Y2021.Day5 (solve) where

import Data.List
import Data.Foldable
import Data.Traversable
import Debug.Trace

import Data.List.Extra (split)

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
compute 1 rs@(r:rs') = res
  where
  finalState = foldl' updateState M.empty rs
  cnt s x = if x > 1 then s + 1 else s
  res = M.foldl' cnt 0 finalState

compute 2 (r:rs) = res
  where
  res = 0

updateState :: St -> Line -> St
updateState m l = {-trace (showM m) $-} foldl' upd m $ fillLine l
  where
  maxed m p = case M.lookup p m of
    Nothing -> False
    Just n  -> n == 2
  upd m p = if maxed m p then m else M.alter ins p m
  ins Nothing = Just 1
  ins (Just n) = Just (n+1)

fillLine :: Line -> [Pt]
fillLine (p1@(x1,y1), p2@(x2,y2))
  | x1 == x2 = [(x1,y) | y <- [y1..y2] ++ [y2..y1]]
  | y1 == y2 = [(x,y1) | x <- [x1..x2] ++ [x2..x1]]
  | otherwise = []

parse :: String -> [Rec]
parse = map readRec . lines

readRec :: String -> Rec
readRec input = ((x1,y1),(x2,y2))
  where
  (p1 : arrow : p2 : _) = words input
  [x1, y1, x2, y2] = map read $ concatMap (split (==',')) [p1,p2]

-- Debug
showM :: St -> String
showM m = show m -- "" -- res
  where
  ps = M.toList m
  maxCol = 9 -- maximum $ map (fst . fst) ps
  maxRow = 9 -- maximum $ map (snd . fst) ps
  res =
    concat $ for [0..maxCol] $ \c ->
      concat $ (for [0..maxRow] $ \r ->
        case M.lookup (r,c) m of
          Nothing -> "."
          Just n  -> show n) ++ ["\n"]

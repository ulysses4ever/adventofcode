{-# language BangPatterns #-}
{-# language TupleSections #-}
module Y2021.Day11 (solve) where

import Aux
import Data.List
import Data.List.Extra (groupSortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Debug.Trace
import Data.Int
import Data.MemoTrie (memo)
import Control.Arrow

-- first arg is Part #
solve :: Int -> String -> IO ()
solve n = print . compute n . parse

type Rec = [Cell]
data St = S
type I = Int8
type Pt = (I, I)
type Cell = (I, I)
type M = M.Map Pt Cell
type Hit = (Pt, I)

compute :: Int -> [Rec] -> Int
compute 1 rs = traceShow states res
  where
  rw = length rs
  cl = length (head rs)
  initial = M.fromList
    [ ((fromIntegral r,fromIntegral c), rs !! r !! c) |
      r <- [0..rw-1], c<-[0..cl-1]]
  iters = 2
  states = take iters $
    iterate (step (fromIntegral rw) (fromIntegral cl)) (initial, 0)
  res = sum $ map snd states

compute 2 rs = res
  where
  res = 0

maxLevel :: Int8
maxLevel = 9

step :: I -> I -> (M, Int) -> (M, Int)
step rw cl (m, n) = (m'' , n + n')
  where
    valid (r,c) = 0 <= r && r < rw && 0 <= c && c < cl
    (flashed, m') = M.mapAccumWithKey
      (\fl pt (e,f) -> if e == maxLevel
        then (pt:fl, (0,f+1))
        else (fl, (e+1,f)))
      [] m
    n' :: Int
    n' = sum $ map (length . snd) steps
    (steps, (m'',_):_) =
      break (not . null . snd) $
      iterate finalizeStep (m', flashed)

    finalizeStep :: (M, [Pt]) -> (M, [Pt])
    finalizeStep (m, flashed) = upd m $ hit flashed

    -- iterate upd (flashed, m)
    -- 1: hit: flashed -> hit;
    -- 2: upd: hit, m  -> flashed', m'
    -- 3: if null flashed' then stop else go to 1
    upd :: M -> [Hit] -> (M, [Pt])
    upd m ht = foldl' updHit (m,[]) ht

    updHit :: (M, [Pt]) -> Hit -> (M, [Pt])
    updHit (m, fl) (p,n) = (m', if x then p:fl else fl)
      where
        (x, m') = M.alterF
          (\(Just (x,f)) -> if x + n > maxLevel
            then (True, Just (0, f+1))
            else (False, Just (x + n, f)))
          p m

    hit :: [Pt] -> [Hit]
    hit flashed =
      map (head &&& (fromIntegral . length)) .
      groupSortOn id .
      filter (\p -> valid p || notElem p flashed) $
      concatMap nhood flashed


nhood :: Pt -> [Pt]
nhood = memo go
  where
    go (r,c) = [(r+dr,c+dc) |
               dr <- [-1..1], dc <- [-1..1],
               dr /= 0 || dc /= 0,
               let r' = r+dr,
               let c' = c+dc]

parse :: String -> [Rec]
parse = map readRec . lines

readRec :: String -> Rec
readRec = map ((,0) . read . pure)

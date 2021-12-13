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
compute 1 rs = trace (intercalate "\n" $ map (showMat "" rw cl) states) res
  where
  rw = length rs
  cl = length (head rs)
  initial = M.fromList
    [ ((fromIntegral r,fromIntegral c), rs !! r !! c) |
      r <- [0..rw-1], c<-[0..cl-1]]
  iters = 2
  states :: [M]
  states = take iters $
    iterate (step (fromIntegral rw) (fromIntegral cl)) initial
  res = sum $ map (fromIntegral . M.foldl' (\s cell -> s + snd cell) 0) states

compute 2 rs = res
  where
  res = 0

maxLevel :: Int8
maxLevel = 9

step :: I -> I -> M -> M
step rw cl m = m''
  where
    valid (r,c) = 0 <= r && r < rw && 0 <= c && c < cl

    (initFlashed, m') = M.mapAccumWithKey
      (\fl pt (e,f) -> if e == maxLevel
        then (pt:fl, (0,f+1))
        else (fl, (e+1,f)))
      [] m

    (_, (m'',_):_) =
      break (not . null . snd) $
      iterate finalizeStep (m', initFlashed)

    finalizeStep :: (M, [Pt]) -> (M, [Pt])
    finalizeStep (m, flashed) = updWithHit m $ hit flashed

    updWithHit :: M -> [Hit] -> (M, [Pt])
    updWithHit m ht = foldl' upd (m,[]) ht
      where
      upd :: (M, [Pt]) -> Hit -> (M, [Pt])
      upd (m, fl) (p,n) = (m', if x then p:fl else fl)
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

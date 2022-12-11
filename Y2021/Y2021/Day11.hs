{-# language BangPatterns #-}
{-# language TypeApplications #-}
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
compute n rs =
  -- trace ("init field:\n" ++ showMat "" rw cl initial)
  res n
  where
  rw = length rs
  cl = length (head rs)
  initial = M.fromList
    [ ((i r,i c), rs !! r !! c) |
      r <- [0..rw-1], c<-[0..cl-1]]
  iters = 100
  states = iterate (step (i rw) (i cl)) (initial, False)
  final = states !! iters
  res 1 = M.foldl' (\s (_, fl) -> s + i @_ @Int fl) (0::Int) . fst $ final
  res 2 = length $ takeWhile (not . snd) states

maxLevel :: Int8
maxLevel = 9

step :: I -> I -> (M, Bool) -> (M, Bool)
step rw cl (m,_) =
  -- trace ("step, result:\n" ++ showMat "" rw cl m'')
    res
  where
    valid (r,c) = 0 <= r && r < rw && 0 <= c && c < cl

    (initFlashed, m') = M.mapAccumWithKey
      (\fl pt (e,f) -> if e == maxLevel
        then (pt:fl, (0,f+1))
        else (fl, (e+1,f)))
      [] m

    res = go (m', [], initFlashed)
    go st@(_,fl,fl') = case finalizeStep st of
      (m', [])  -> (m', length (fl++fl') == i rw * i cl)
      (m', fl'') -> go (m', fl' ++ fl, fl'')

    finalizeStep :: (M, [Pt], [Pt]) -> (M, [Pt])
    finalizeStep (m, fl, fl') = updWithHit m $ hit fl fl'

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

    -- calculate who was hit by the flashed guys and how many times
    hit :: [Pt] -> [Pt] -> [Hit]
    hit fl fl' =
      map (head &&& (fromIntegral . length)) .
      groupSortOn id .
      filter (\p -> valid p && p `notElem ` fl && p `notElem ` fl') $
      concatMap nhood fl'


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

i :: (Integral a, Num b) => a -> b
i = fromIntegral

{-# language BangPatterns #-}
module Y2021.Day21 (solve) where

import Aux
import Data.List
import Debug.Trace

-- first arg is Part #
solve :: Int -> String -> IO ()
solve n = print . compute n . parse

type Rec = Int
data St = S

compute :: Int -> [Rec] -> Int
compute 1 [s1, s2] = res
  where
    movesUntilWin = length . takeWhile (<1000)
    n1 = movesUntilWin $ scores 1 s1
    n2 = movesUntilWin $ scores 2 s2
    (n, sc) = if n1 < n2
      then (n1, scores 2 s2)
      else (n2, scores 1 s1)
    rolls = (2*n + 1) * 3
    loserMoves = sc !! (n - 1)
    res = rolls * loserMoves

compute 2 rs = res
  where
  res = 0

dice p = (1+2+3+ if p == 2 then 9 else 0) : (map ((\n -> if n == 0 then 10 else n) . (`mod` 10) . (+ 18)) $ dice p)
moves p s = tail $ scanl (\su d -> let n = (su + d) `mod` 10 in if n == 0 then 10 else n) s $ dice p
scores p s = scanl1 (+) $ moves p s

parse :: String -> [Rec]
parse = map readRec . lines

readRec :: String -> Rec
readRec = read . pure . last

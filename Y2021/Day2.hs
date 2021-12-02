{-# language BangPatterns #-}
{-# language LambdaCase   #-}
module Y2021.Day2 (solve1, solve2) where

import Data.List

solve1 :: String -> IO ()
solve1 = print . compute . parse

compute rs = d * w
  where
  (S d w) = foldl' exec (S 0 0) rs

data Cmd = F | U | D

data St = S !Int !Int

exec :: St -> Rec -> St
exec (S d w) (R c n) = case c of
  F -> S d (w+n)
  U -> S (d-n) w
  D -> S (d+n) w 

readCmd :: String -> Cmd
readCmd "forward" = F
readCmd "up" = U
readCmd "down" = D

data Rec = R Cmd Int

parse :: String -> [Rec]
parse = map (readRec . words) . lines

readRec [cmp, arg] = R (readCmd cmp) (read arg) 

solve2 :: String -> IO ()
solve2 input = print 0


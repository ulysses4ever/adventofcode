{-# language BangPatterns #-}
{-# language LambdaCase   #-}
module Y2021.Day2 (solve) where

import Data.List

-- first arg is Part
solve :: Int -> String -> IO ()
solve n = print . compute n . parse

compute 1 rs = d * w
  where
  (S d w) = foldl' exec (S 0 0) rs

compute 2 rs = d * w
  where
  (S' a d w) = foldl' exec' (S' 0 0 0) rs

data Cmd = F | U | D
data St = S !Int !Int
data St' = S' !Int !Int !Int

exec :: St -> Rec -> St
exec (S d w) (R c n) = case c of
  F -> S d (w+n)
  U -> S (d-n) w
  D -> S (d+n) w 

exec' :: St' -> Rec -> St'
exec' (S' a d w) (R c n) = case c of
  F -> S' a (d+a*n) (w+n)
  U -> S' (a-n) d w
  D -> S' (a+n) d w 

data Rec = R Cmd Int

readCmd :: String -> Cmd
readCmd "forward" = F
readCmd "up" = U
readCmd "down" = D

parse :: String -> [Rec]
parse = map (readRec . words) . lines

readRec [cmp, arg] = R (readCmd cmp) (read arg) 


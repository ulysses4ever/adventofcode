{-# language LambdaCase #-}
{-# language BangPatterns #-}
module Y2021.Day25 (solve) where

import Aux
import Data.List
import Debug.Trace
import qualified Data.IntMap.Strict as I

-- first arg is Part #
solve :: Int -> String -> IO ()
solve n = print . compute n . parse

data C = E | S -- east or south
  deriving Eq
type M = I.IntMap C
data St = St M Int Int
  deriving Show

compute :: Int -> St -> Int
compute 1 rs = res
  where
  res = 0

compute 2 rs = res
  where
  res = 0

step :: St -> Maybe St
step (St m rw cl) = Just $ St m' rw cl
  where
    (_, m', changed) = I.foldlWithKey' (upd E) (m, I.empty, False) m
    upd c s@(m, m', ch) n c'
      | c == c' =
        if nxt `I.member` m
        then def
        else (m, I.insert nxt c m', True)
      | otherwise = def
      where
        def = (m, I.insert n c' m', ch)
        nxt = next c rw cl n

next E rw cl n
  | n `mod` cl == cl - 1 = n - cl + 1
  | otherwise = n + 1
next S rw cl n
  | n `div` cl == rw - 1 = n `mod` cl
  | otherwise = n + cl

parse :: String -> St
parse s = St m rw cl
  where
    st@(m,rw,cl) = readIMat (/= '.') (\case '>' -> E; 'v' -> S) s
    res = St m rw cl

showSt (St m rw cl) = showIMat "" "." (m,rw,cl)

instance Show C where
  show E = ">"
  show S = "v"

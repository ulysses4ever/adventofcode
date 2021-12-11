{-# language BangPatterns #-}
module Y2021.Day10 (solve) where

import Aux
import Data.List
import Debug.Trace

-- first arg is Part #
solve :: Int -> String -> IO ()
solve n = print . compute n . parse

type Rec = String
type St = String

compute :: Int -> [Rec] -> Int
compute 1 rs = res
  where
  res = sum $ map (balance []) rs

compute 2 rs = res
  where
  res = 0

op = "([{<"
cl c = case c of
  '(' -> ')'
  '[' -> ']'
  '{' -> '}'
  '<' -> '>'

balance :: St -> Rec -> Int
balance stk (c:cs)
  | c `elem` op = balance (c:stk) cs
  | otherwise   = case stk of
      (s:stk') -> if c == cl s then balance stk' cs else cost c
      _        -> 0
balance _ _ = 0

cost c = case c of
  ')' -> 3
  ']' -> 57
  '}' -> 1197
  '>' -> 25137

parse :: String -> [Rec]
parse = lines

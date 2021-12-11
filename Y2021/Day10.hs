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
compute n rs = if n == 1 then res1 else res2
  where
  res1 = sum $ map (balance []) rs
  rs'  = filter ((== 0) . balance []) rs
  cs = map (compl []) rs'
  res2 = winner $ map score cs

winner :: [Int] -> Int
winner ss = sort ss !! (length ss `div` 2)

compl :: St -> Rec -> St
compl stk (c:cs)
  | c `elem` op = compl (c:stk) cs
  | otherwise   = case stk of
      (s:stk') -> if c == cl s then compl stk' cs else error "illegal"
      _        -> error "illegal"
compl s _ = map cl s

score :: St -> Int
score = foldl' (\z c -> z * 5 + s c) 0
  where
    s ')' = 1
    s ']' = 2
    s '}' = 3
    s '>' = 4
    s _   = error "unexpected"

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

{-# language BangPatterns #-}
module Y2021.Day10 (solve) where

import Aux
import Data.List
import Debug.Trace
import Data.Either (lefts, rights)

-- first arg is Part #
solve :: Int -> String -> IO ()
solve n = print . compute n . parse

type Rec = String
type St = String

compute :: Int -> [Rec] -> Int
compute n rs = if n == 1 then res1 else res2
  where
  rs' = map (balance []) rs
  res1 = sum $ lefts rs'
  res2 = winner $ map score $ rights rs'

winner :: [Int] -> Int
winner ss = sort ss !! (length ss `div` 2)

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

balance :: St -> Rec -> Either Int St
balance stk (c:cs)
  | c `elem` op = balance (c:stk) cs
  | otherwise   = case stk of
      (s:stk') -> if c == cl s then balance stk' cs else Left $ cost c
      _        -> error "illegal"
balance s _ = Right $ map cl s

cost c = case c of
  ')' -> 3
  ']' -> 57
  '}' -> 1197
  '>' -> 25137

parse :: String -> [Rec]
parse = lines

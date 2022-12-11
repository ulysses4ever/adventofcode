#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra
-}
{-# language TupleSections #-}
{-# language BangPatterns #-}

import Flow ((.>), (<|), (|>))
import Data.List

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
-- part :: Int -> ??? -> Int
part n i = i -- map (score n) .> sum
  where
    res = foldl' move (P 0 0, P 0 0, []) i

move (h, t, ps) (m, n) = (h', t', ps)
  where
  d = h <-> t
  ma = m <&> m
  h' = h <+> (n <.> m)
  j = rot ma <&> d
  tlen = n - 1 + (d <#> m)
  t' = t <+> j <+> (tlen <.> m)

-- Read one line of problem's input into something more structured
-- parseLine :: String -> []
parseLine = words .> \
    [d, n] -> let i = read n :: Int in
      (,i) <| case d of
        "L" -> P (-1) 0
        "R" -> P 1 0
        "U" -> P 0 1
        "D" -> P 0 (-1)


data Pair = P !Int !Int
  deriving Show

(P x y) <+> (P u v) = P (x+u) (y+v)
(P x y) <-> (P u v) = P (x-u) (y-v)
(P x y) <#> (P u v) = x*u + y*v
(P x y) <&> (P u v) = P (x*u) (y*v)
n <.> (P u v) = P (n*u) (n*v)
rot (P u v) = P v u

{--------------------------------------------------------
--
-- Below is somewhat standard stuff, which is moslty reused
-- from day to day.
--
--------------------------------------------------------}

-- Entry point: read stdin, solve the problem and print the result
main  = interact (solve .> show)

-- Solve both parts and return a list with two elements -- the results
-- Input: problem's full text
solve input = (part <$> [1]) <*> pure (parse input)

-- Turn problem's full text into something more structured
-- parse :: String -> ???
parse =  lines .> map parseLine

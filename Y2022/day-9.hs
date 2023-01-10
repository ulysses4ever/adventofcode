#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra, linear
-}
{-# language TupleSections #-}
{-# language BangPatterns #-}
{-# language PatternSynonyms #-}

import Flow ((.>), (<|), (|>))
import Data.List
import Data.List.Extra (nubOrd)
import Linear.Vector
import Linear.V2
import Debug.Trace

type P = V2 Int
pattern P x y = V2 x y

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
-- part :: Int -> ??? -> Int
part n i = nubOrd tr |> length -- map (score n) .> sum
  where
    (h,t,tr) = foldl' moveWithTrace (P 0 0, P 0 0, []) i

type State = (P, P, [P])
type Move = (P, Int)

moveWithTrace :: State -> Move -> State
moveWithTrace (h, t, ps) mv@(m,n) = traceShow (h',t') (h',t',ps')
  where
    h' = h + (n <.> m)
    t' = moveTail h t mv
    ps' = runTrace t t' ++ ps

moveTail :: P -> P -> Move -> P
moveTail h t (m, n) = t'
  where
  d = h - t
  ma = m <&> m
  j = rot ma <&> d
  tlen = n - 1 + (d <#> m)
  t' = t + (min tlen 1 <.> j) + (tlen <.> m)

(P x y) <#> (P u v) = x*u + y*v
(P x y) <&> (P u v) = P (x*u) (y*v)
n <.> (P u v) = P (n*u) (n*v)
rot (P u v) = P v u

runTrace (P x1 y1) (P x2 y2) =
  [P x y |
   x <- [min x1 x2 .. max x1 x2],
   y <- [min y1 y2 .. max y1 y2]
         ]

-- Read one line of problem's input into something more structured
-- parseLine :: String -> []
parseLine = words .> \
    [dir, n] -> let i = read n :: Int in
      (,i) <| case dir of
        "L" -> l
        "R" -> r
        "U" -> u
        "D" -> d

l, r, u, d :: P
l = P (-1) 0
r = P 1 0
u = P 0 1
d = P 0 (-1)

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

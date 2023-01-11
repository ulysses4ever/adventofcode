#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra, linear, lens, containers
-}
{-# language TupleSections #-}
{-# language BangPatterns #-}
{-# language PatternSynonyms #-}

import Flow ((.>), (<|), (|>))
import Data.List
import Data.List.Extra (nubOrd)
import Linear.Vector
import Linear.V2
import Linear.Metric (dot)
import Debug.Trace
import Control.Arrow ((&&&))
import Control.Lens.Operators ((^.))
import qualified Data.Set as S

type P = V2 Int
pattern P x y = V2 x y

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
-- part :: Int -> ??? -> Int
part n = simulate .> nubOrd .> length
  where

type State = (P, P, [P])
type Dir = P
type Move = (Dir, Int)

simulate ps = let
  (_,_,tr) = foldl' moveWithTrace (zero, zero, [zero]) ps
  in tr

moveWithTrace :: State -> Move -> State
moveWithTrace (h, t, ps) mv@(d,n) = -- traceShow (h',t')
  (h',t',ps')
  where
    trace = take (n+1) $ iterate (moveOneStep d) (h,t)
    (h',t') = last trace
    ps' = map snd trace ++ ps

moveOneStep :: Dir -> (P, P) -> (P, P)
moveOneStep d (h, t) = (h + d, moveTailOneStep h t d)

moveTailOneStep :: P -> P -> Dir -> P
moveTailOneStep h t d
  | (h - t) `dot` d == 1 = h
  | otherwise = t


--
-- Failed attempts
--

-- this looks right but the trace business didn't quite work somehow
moveTail :: P -> P -> Move -> P
moveTail h t (m, n) = t'
  where
  d = h - t
  ma = m <&> m
  j = rot ma <&> d
  tlen = n - 1 + (d <#> m)
  t' = t + (min tlen 1 <.> j) + (tlen <.> m)

(<#>) = dot
(<&>) = (*)
(<.>) = (*^)
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


--
--  Debug Utils
--

minMax2D ps = ((xmin, xmax), (ymin, ymax))
  where
    mnmx ps lens = ps
      |> map (\v -> v ^. lens)
      |> (minimum &&& maximum)
    mnmxVal@[(xmin, xmax), (ymin, ymax)] = map (mnmx ps) [_x, _y]

showPoints :: [P] -> String
showPoints ps = unlines $ foldl'
    (\ls r ->
      ls ++ (pure $ foldl' (\cs c ->
        cs ++ pure (if V2 c r `S.member` psSet then '#' else '.'))
      [] [xmin..xmax]
      ))
    [] (reverse [ymin..ymax])
  where
    ((xmin, xmax), (ymin, ymax)) = minMax2D ps
    psSet = S.fromList ps

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

#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra, containers
-}
{-# language LambdaCase #-}

import Flow ((.>), (|>))
import Data.List
import Data.Set (Set)
import qualified Data.Set as S

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
-- part :: Int -> ??? -> Int
part n i = res n
  where
    y = 2000000
    beaconsOnLine = map snd i |> filter ((== y) . snd) |> map fst
    res n = S.size $
      foldl' collect S.empty i `S.difference` S.fromList beaconsOnLine
    collect ss (s,b) = S.fromList (isect y s (dist s b)) `S.union` ss

dist (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)

-- intersect the given neighbourhood with a given y-line
isect yline (x,y) len
  | dy <= len = [x - dx .. x + dx]
  | otherwise = []
  where
    dy = abs (yline - y)
    dx = abs (len - dy)

type P = ((Int,Int),(Int,Int))

-- Read one line of problem's input into something more structured
parseLine :: String -> P
parseLine = words .> \case
  ["Sensor", "at", xs, ys, "closest", "beacon", "is", "at", xb, yb] ->
    ((xs', ys'), (xb', yb'))
        where
        xs' = parse xs
        ys' = parse ys
        xb' = parse xb
        yb' = yb |> drop 2 |> read
        parse = init .> drop 2 .> read


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
solve input = (part <$> [1,2]) <*> pure (parse input)

-- Turn problem's full text into something more structured
-- parse :: String -> ???
parse =  lines .> map parseLine

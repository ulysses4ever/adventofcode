#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra, data-interval, extended-reals
-}
{-# language LambdaCase #-}
{-# language NumericUnderscores #-}

import Flow ((.>), (|>))
import Data.List
import Data.IntervalSet as S
import Data.Interval as I
import Data.ExtendedReal
import Debug.Trace
import Data.Maybe

type P = (Int,Int)
type PP = (P,P)

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
part :: Int -> [PP] -> Int
part 1 = setSize . noBeacons y
  where
    y = --10
      2_000_000
part 2 = go 0
  where
    ub = --20
      4_000_000
    target = S.singleton $ Finite 0 <=..<= Finite ub
    go :: Int -> [PP] -> Int
    go y pp
      | y == ub+1 = error "Didn't find the spot"
      | otherwise = case S.toList diff of
          []  -> go (y+1) pp
          [i] -> extractSinglePoint i
            |> (* ub)
            |> (+ y)
          _   -> error "Non-unique solution"
      where
        noB = noBeacons y pp
        diff = target `S.difference` noB

extractSinglePoint :: Interval Int -> Int
extractSinglePoint i
  | (l, lb) <- lowerBound' i,
    (r, rb) <- upperBound' i = let
      (n, upd) = case (lb,rb) of
        (Open, Open)   -> (l, (+1))
        (Closed, Open) -> (l, id)
        (Open, Closed) -> (r, id)
      n' = case n of { Finite n' -> n'; _ -> error "extractSinglePoint: infinity" }
      in upd n'

setSize = sum . map I.width . S.toList

noBeacons y = foldl' collect S.empty
  where
    collect ss (s,b) = let
      new = isect y s (dist s b)
      res = new `S.insert` ss
      in -- traceShow res $ traceShow new
        res


dist :: P -> P -> Int
dist (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)

-- intersect the given neighbourhood with a given y-line
isect :: Int -> P -> Int -> I.Interval Int
isect yline (x,y) len
  | dy <= len = res
  | otherwise = I.empty
  where
    dy = abs (yline - y)
    dx = abs (len - dy)
    res = Finite (x - dx) <=..<= Finite (x + dx)

-- Read one line of problem's input into something more structured
parseLine :: String -> PP
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

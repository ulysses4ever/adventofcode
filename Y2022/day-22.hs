#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra, linear, array
-}
{-# language LambdaCase #-}
{-# language MultiWayIf #-}
{-# language BlockArguments #-}
{-# language PartialTypeSignatures #-}
{-# language PatternSynonyms #-}

import Flow ((.>), (|>))
import Data.List
import Data.Maybe
import Data.Char
import Data.Function
import Data.Array
import Debug.Trace

import Linear.Vector
import Linear.V2

type P = V2 Int
pattern P x y = V2 x y

type A = Array P Char
data Cmd = Steps Int
  | Turn Char
  deriving Show
type Input = (A, [Cmd])

type Dir = P
type State = (P, Dir)

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
-- part :: Int -> Input -> _
part p (map', cs) = res finish
  where
    colstart = elems map' |> takeWhile (== ' ') |> length
    (_, maxP) = bounds map'
    maxP' = (+1) <$> maxP
    finish = foldl' exec (P 0 colstart, ri) cs

    res (P r c, dir) = 1000 * (r+1) + 4 * (c+1) + weight dir

    weight dir = if
      | dir == ri -> 0
      | dir == dw -> 1
      | dir == le -> 2
      | dir == up -> 3

    exec st@(pos, dir) = -- traceShow st $
      \case
        Steps n -> (steps n st, dir)
        Turn t -> (pos, turn dir t)

    steps n (pos, dir) = go n pos where
      go 0 pos = pos
      go n pos = case map' ! pos' of
        '#' -> pos
        '.' -> go (n-1) pos'
        _   -> error "impossible: steps into unknown territory"
        where
          inc pos = (pos + dir) `modVec` maxP'
          pos' = iterate inc pos
            |> tail
            |> dropWhile ((map' !) .> (== ' '))
            |> head

    turn dir t = iterate perp dir !! case t of
      'L' -> 1
      'R' -> 3

modVec (P x1 y1) (P x2 y2) = P (x1 `mod` x2) (y1 `mod` y2)

up = P (-1) 0
dw = P 1 0
le = P 0 (-1)
ri = P 0 1

parse :: String -> Input
parse inp = (map', commands')
  where
    (mapRows, empty:commands:[]) = span (not . null) $ lines inp
    commands' = flip unfoldr commands \case
      [] -> Nothing
      cs -> case span isDigit cs of
        ([], c:rest) -> Just (Turn c, rest)
        (digits, rest) -> Just (Steps $ read digits, rest)
    cls = maximum $ map length mapRows
    mapRows' = map pad mapRows
    pad l = l ++ replicate (cls - length l) ' '
    map' = listArray
      (P 0 0, P (length mapRows - 1) (cls - 1))
      (concat mapRows')

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

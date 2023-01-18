#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra, linear, unordered-containers
-}
{-# language LambdaCase #-}
{-# language PatternSynonyms #-}

import Flow ((.>), (|>))
import Data.List
import Data.Either
import Data.List.Extra (chunksOf, split)
import Data.Foldable.Extra (notNull)
import Control.Arrow ((***))

import qualified Data.HashMap.Strict as M

import Debug.Trace

import Linear.Vector
import Linear.V2
type P = V2 Int
pattern P x y = V2 x y

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
-- part :: Int -> ??? -> Int
part n inp = ms
  where
    ms@(lx, ly) = concat inp
      |> partitionEithers
      |> (make_maps *** make_maps)
    make_maps = M.fromListWith (++)

type Seg = (Int,Int)
type KSegs = (Int, [Seg])

-- Read one line of problem's input into something more structured
parseLine :: String -> [Either KSegs KSegs]
parseLine = split (`elem` ", ->")
  .> filter notNull
  .> chunksOf 2
  .> (\ws -> zip ws $ tail ws)
  .> map collect
  where
    collect ([sx1, sy1], [sx2, sy2])
      |x1 == x2 = Left (x1, pure (min y1 y2, max y1 y2))
      |y1 == y2 = Right (y1, pure (min x1 x2, max x1 x2))
      | otherwise = error "impossible: line is not axis aligned"
      where
        x1 = read sx1
        x2 = read sx2
        y1 = read sy1
        y2 = read sy2

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

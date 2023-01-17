#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra, linear
-}
{-# language LambdaCase #-}
{-# language PatternSynonyms #-}

import Flow ((.>), (|>))
import Data.List
import Data.List.Extra (chunksOf, split)
import Data.Foldable.Extra (notNull)

import Debug.Trace

import Linear.Vector
import Linear.V2
type P = V2 Int
pattern P x y = V2 x y

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
-- part :: Int -> ??? -> Int
part n = id -- map (score n) .> sum

-- Read one line of problem's input into something more structured
parseLine :: String -> [P]
parseLine = split (`elem` ", ->")
  .> filter notNull
  .> chunksOf 2
  .> map (\[x, y] -> P (read x) (read y))

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

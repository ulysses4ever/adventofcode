#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra
-}
{-# language LambdaCase #-}

import Flow ((.>))
import Data.List

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
-- part :: Int -> ??? -> Int
part p = tails .> map (take n .> nub) .> takeWhile (length .> (< n)) .> length .> (+ n)
  where
  n = if p == 1 then 4 else 14

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
solve input = (part <$> [1,2]) <*> (pure $ parse input)

-- Turn problem's full text into something more structured
-- parse :: String -> ???
parse =  id

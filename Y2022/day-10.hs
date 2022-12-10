#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra
-}
{-# language LambdaCase #-}

import Flow ((.>), (|>))
import Data.List

data Cmd = A Int | N

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
-- part :: Int -> ??? -> Int
part n i = res
  where
    history = foldl' f (1, [0]) i |> (\(x,hist) -> reverse $ x:hist)
    interestingCycles = [20,60,100,140,180,220]
    res = map (\c -> c * history !! c) interestingCycles |> sum
    f (x, hist) = \case
      A i -> (x + i, x:x:hist)
      N   -> (x, x:hist)

-- Read one line of problem's input into something more structured
-- parseLine :: String -> ???
parseLine = words .> p
  where
    p [a, i] = A $ read i
    p [n]    = N

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

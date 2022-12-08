#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra
-}
{-# language LambdaCase #-}

import Flow ((.>), (|>))
import Data.List
import Data.Maybe
import Debug.Trace

type State = ([Int], [Int])

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
-- part :: Int -> ??? -> Int
part n i = res n
  where
    res 1 = sizes |> filter (<= 100000) |> sum
    res 2 = head $ dropWhile (< need) $ sort sizes

    sizes = buildSizes i

    use = last sizes
    req = 30000000
    ava = 70000000
    fre = ava - use
    need = req - fre

buildSizes = tail .> foldl' collect ([], [], ["/"]) .> complete'
  where
    complete []     _ = error "impossible"
    complete (o:os) c = (c+o):os

    complete' ([last], closed) = closed

    collect :: State -> [String] -> State
    collect s@(cur:opened, closed) = \case
      ["$", "ls"] -> s

      ["$", "cd", ".."] -> (complete opened cur, cur:closed)

      ["$", "cd", name] -> (0:cur:opened, closed)

      ["dir", name] -> s

      [size, name] -> (cur + read size : opened, closed)

-- Read one line of problem's input into something more structured
-- parseLine :: String -> ???
parseLine = words

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
parse =  lines .> map parseLine

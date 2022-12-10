#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra
-}
{-# language LambdaCase #-}

import Flow ((.>), (|>))
import Data.List
import Data.List.Extra

data Cmd = A Int | N

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
part n i = res n
  where
    interestingCycles = [20,60,100,140,180,220]
    history = foldl' f (1, []) i |> snd |> reverse
    f (x, hist) = \case
      A i -> (x + i, x:x:hist)
      N   -> (x, x:hist)

    res 1 = map (\c -> c * history !! (c-1)) interestingCycles
      |> sum
      |> show
    res 2 = zip (cycle [0..39]) history
      |> map (\(c, s) ->
        if abs (c - s) < 2 then '#' else '.')
      |> chunksOf 40
      |> unlines

-- Read one line of problem's input into something more structured
parseLine :: String -> Cmd
parseLine = words .> \case
    [a, i] -> A $ read i
    [n]    -> N

{--------------------------------------------------------
--
-- Below is somewhat standard stuff, which is moslty reused
-- from day to day.
--
--------------------------------------------------------}

-- Entry point: read stdin, solve the problem and print the result
main  = getContents >>= (solve .> mapM_ putStrLn)

-- Solve both parts and return a list with two elements -- the results
-- Input: problem's full text
solve input = (part <$> [1,2]) <*> pure (parse input)

-- Turn problem's full text into something more structured
-- parse :: String -> ???
parse =  lines .> map parseLine

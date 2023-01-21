#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra, linear, array
-}
{-# language LambdaCase #-}
{-# language BlockArguments #-}
{-# language PatternSynonyms #-}

import Flow ((.>), (|>))
import Data.List
import Data.Maybe
import Data.Char
import Data.Array

import Linear.Vector
import Linear.V2

type P = V2 Int
pattern P x y = V2 x y

type A = Array P Char
data Cmd = Steps Int
  | Turn Char
  deriving Show

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
-- part :: Int -> ??? -> Int
part p = id -- map (score n) .> sum

-- Read one line of problem's input into something more structured
parse :: String -> (A, [Cmd])
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

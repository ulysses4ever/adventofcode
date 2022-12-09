#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra
-}
{-# language LambdaCase #-}

import Flow ((.>), (|>))
import Data.List
import Data.List.Extra
import Data.Int
import Debug.Trace

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
-- part :: Int -> ??? -> Int
part n i = res n
  where
  res 1 = length visible
  res 2 = zipWith (*) (getScores j) (getScores j') |> maximum

  visible = nubOrd . map fst $
    concatMap visibleOnLine j ++ concatMap visibleOnLine j'

  getScores inp = concatMap scenicScoresOnLine inp  |> sortOn fst |> map snd

  -- put unique index on every cell
  w = length $ head i
  j = concat i |> zip [1..] |> chunksOf w
  j' = transpose j

visibleOnLine l = visibleOneDirection l ++ visibleOneDirection (reverse l)
  where
  visibleOneDirection = snd . foldl'
    (\ s@(mx, ps) p@(_, a) ->
      if mx < a then (a, p:ps) else s)
    (-1, [])

scenicScoresOnLine = go ([], [])
  where
  go (past, scores) [] = scores
  go (past, scores) (p@(i, x) : next) = go (p:past, s:scores) next
    where
    s = (i, getScoreOneDirection x past * getScoreOneDirection x next)

  getScoreOneDirection x l = case span (< x) (map snd l) of
    (ps, []) -> length ps
    (ps,  _) -> length ps + 1


-- Read one line of problem's input into something more structured
parseLine :: String -> [Int8]
parseLine = map (pure .> read)

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

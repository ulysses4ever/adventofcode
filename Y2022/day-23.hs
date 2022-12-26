#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra, containers, linear, lens
-}
{-# language LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

import Flow ((.>), (|>))
import Data.List
import Data.List.Extra (groupOn)
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as M
import Linear.Vector
import Linear.V2
import Control.Arrow ((&&&))
import Control.Lens.Operators ((^.))
import Debug.Trace

type P = V2 Int

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
-- part :: Int -> [P] -> Int
part p ps = -- range
  finalElfs
  where
    finalElfs = sort $ goRounds 1 (cycle schdPattern) ps
    goRounds 0 _ ps = ps
    goRounds i schedule ps = goRounds (i-1) (tail schedule) ps'
      where
        psSet = S.fromList ps
        props = M.fromList
          $ map (\(e, (Just e')) -> (e, e'))
          $ concat
          $ filter (length .> (== 1))
          $ groupOn snd
          $ filter (not . isNothing . snd)
          $ zip ps
          $ map propose ps
        propose :: P -> Maybe P
        propose elf
          =   (+ elf)
          <$> (listToMaybe
          $   filter (canStep elf) (take 4 schedule))
        canStep :: P -> P -> Bool
        canStep elf dir = let
          p = perp dir
          s = elf + dir
          in all (`S.notMember` psSet) [s, s + p, s - p]
        ps' = map move ps
        move elf = case props M.!? elf of
          Just elf' -> elf'
          Nothing   -> elf

    schdPattern = [n,s,w,e]

--range :: [P] -> (Int,Int)
range ps = (xmax - xmin + 1, ymax - ymin + 1)
  where
    mnmx ps lens = ps
      |> map (\v -> v ^. lens)
      |> (minimum &&& maximum)
    mnmxVal@[(xmin, xmax), (ymin, ymax)] = map (mnmx ps) [_x, _y]


parse inp = res
  where
    ils = zip [0..] $ lines inp
    ipos = concat $ flip map ils \(row, line) ->
      zip (zip (repeat row) [0..]) line
    res = filter (snd .> (== '#')) ipos |> map (fst .> uncurry V2)

n,s,w,e :: P
n = V2 (-1)    0
s = V2    1    0
w = V2    0 (-1)
e = V2    0    1

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

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
part p ps = res p
  -- finalElfs
  -- seq finalElfs 1
  where
    res 1 | (xrg, yrg) <- range finalElfs = xrg * yrg - length ps
    res 2 = 0

    finalElfs = sort $ goRounds 10 (cycle schdPattern) ps
    goRounds 0 _ ps = ps -- trace (showPoints ps) ps
    goRounds i schedule ps = -- trace (showPoints ps) $
      goRounds (i-1) (tail schedule) ps'
      where
        psSet = S.fromList ps
        props = M.fromList
          $ map (\(e, (Just e')) -> (e, e'))
          $ concat
          $ filter (length .> (== 1))
          $ groupOn snd
          $ sortOn snd
          $ filter (not . isNothing . snd)
          $ zip ps
          $ map propose ps
        propose :: P -> Maybe P
        propose elf
          | needNoStep elf = Nothing
          | otherwise = (+ elf)
          <$> (listToMaybe
          $   filter (canStep elf) (take 4 schedule))
        needNoStep elf = all (`S.notMember` psSet) (nhood elf)
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

nhood :: P -> [P]
nhood p = (+ p) <$>
  [ V2 x y | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0 ]

range :: [P] -> (Int,Int)
range ps = (xmax - xmin + 1, ymax - ymin + 1)
  where
    ((xmin, xmax), (ymin, ymax)) = minMax2D ps

minMax2D ps = ((xmin, xmax), (ymin, ymax))
  where
    mnmx ps lens = ps
      |> map (\v -> v ^. lens)
      |> (minimum &&& maximum)
    mnmxVal@[(xmin, xmax), (ymin, ymax)] = map (mnmx ps) [_x, _y]

showPoints :: [P] -> String
showPoints ps = unlines $ foldl' -- traceShow ps $
    (\ls r ->
      ls ++ (pure $ foldl' (\cs c ->
        cs ++ pure (if V2 r c `S.member` psSet then '#' else '.'))
      [] [ymin..ymax]))
    [] [xmin..xmax]
  where
    ((xmin, xmax), (ymin, ymax)) = minMax2D ps
    psSet = S.fromList ps

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

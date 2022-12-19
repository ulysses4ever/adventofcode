#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra, linear, containers
-}
{-# language LambdaCase #-}
{-# language TupleSections #-}

import Flow ((.>), (|>))
import Data.List
import Data.List.Extra
import Data.Maybe
import qualified Data.Set as S
import Data.Set (Set)
import Linear.Vector
import Linear.V3
import Debug.Trace

type V = V3 Int

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
-- part :: Int -> [V] -> Int
part n inp = res n
  where
    ps = S.fromList inp
    res 1 = foldl' f (S.size ps * 6)  ps
    res 2 = res 1 - 6 * countTrapped ps
    f n p =  countNeighboursInSet p ps |> (n -)

countTrapped ps = S.size . snd . foldl' g (S.empty, S.empty) $ ps
  where
    g state = neighbours
      .> foldl' (notVisitedOrTrapped ps) state

countNeighboursInSet p ps
      =  neighbours p
      |> filter (`S.member` ps)
      |> length

notVisitedOrTrapped ps state@(visited, trapped) n
  | n `S.member` visited = (visited, trapped)
  | countNeighboursInSet n ps == 6
    && n `S.notMember` ps = traceShow n $ visited' trapped'
  | otherwise = visited' trapped
    where
      visited' = (S.insert n visited,)
      trapped' = S.insert n trapped

neighbours p = map (p ^+^) basis ++ map (p ^-^) basis

-- Read one line of problem's input into something more structured
parseLine :: String -> V
parseLine = split (== ',') .> \case
  [x,y,z] -> V3 (read x) (read y) (read z)

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

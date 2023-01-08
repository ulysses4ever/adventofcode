#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra, linear, containers, lens, pretty-simple, search-algorithms, transformers
-}
{-# language LambdaCase #-}

import Algorithm.Search
import Flow ((.>), (|>))
import Data.Foldable
import Data.List
import Data.List.Extra
import Data.Maybe
import qualified Data.Set as S
import Data.Set (Set)
import Linear.Vector
import Linear.V3
import Debug.Trace
import Control.Lens.Operators ((^.))
import Control.Arrow ((&&&))
import Text.Pretty.Simple
import Control.Monad.Trans.RWS.CPS

type V = V3 Int

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
-- part :: Int -> [V] -> Int
part n ps = res n
  where

    res 1 = length $ surf
    res 2 =
      length $ filter (`S.member` trapped) surf

    surf = surface' ps
    (water, trapped) = waterAndTrapped ps surf

surface' :: [V] -> [V]
surface' ps = concatMap neighbors ps
  |> filter (`S.notMember` S.fromList ps)

data Surface = Water | Trapped

waterAndTrapped :: [V] -> [V] -> (Set V, Set V)
waterAndTrapped ps surf = foldl' upd (S.empty, S.empty) surf
  where
    psSet = S.fromList ps
    (vl,vh) = range psSet
    upd s@(trapped, water) p = case walk trapped water p of
      Left  newTrapped -> (trapped `S.union` S.fromList newTrapped, water)
      Right newWater   -> (trapped, water `S.union` S.fromList newWater)
    walk :: Set V -> Set V -> V -> Either [V] [V]
    walk trapped water start = case execRWS (bfsM next final start) () Trapped of
      (Trapped, newTrapped) -> Left $ start:newTrapped
      (Water, newWater)     -> Right $ start:newWater
      where
        next p = censor (p :) $
          pure (neighbors p |> filter (`S.notMember` psSet))
        final p
          | not (inRange vl vh p) || p `S.member` water = do
              put Water
              pure True
          | p `S.member` trapped = pure True
          | otherwise = pure False

inRange vl vh p = vl `cmpVec` p && p `cmpVec` vh
cmpVec v1 v2 = and $ zipWith (<=) (toList v1) (toList v2)

range :: Set V -> (V,V)
range ps = (vl, vh)
  where
    vecFromList [x,y,z] = V3 x y z
    mnmx ps lens = ps
      |> S.map (\v -> v ^. lens)
      |> (minimum &&& maximum)
    rs = map (mnmx ps) [_x, _y, _z]
    vl@(V3 xl yl zl) = map fst rs |> vecFromList
    vh@(V3 xh yh zh) = map snd rs |> vecFromList
neighbors p = map (p ^+^) basis ++ map (p ^-^) basis

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

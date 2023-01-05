#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra, linear, containers, lens, pretty-simple, search-algorithms, transformers
-}
{-# language LambdaCase #-}
{-# language TypeApplications #-}
{-# language TupleSections #-}

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
import Data.Sequence (fromList, Seq(..), (><))
import Text.Pretty.Simple
import Control.Monad.Trans.Writer.CPS

type V = V3 Int

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
-- part :: Int -> [V] -> Int
part n inp = res n
  where
    ps = S.fromList inp

    res 1 = surface ps
    res 2 = surface (water ps) -- - rangeSize

waterAndTrapped :: [V] -> ([V], [V])
waterAndTrapped ps = undefined
  where
    psSet = S.fromList ps
    start = concatMap neighbours ps
    res = foldl' step (S.empty, S.empty, S.empty) start
    upd s@(trapped, water, visited) p = case walk trapped water p of
      Left  newTrapped -> (trapped `S.merge` S.fromList newTrapped, water)
      Right newWater   -> (trapped, water `S.merge` S.fromList newWater, visited')
      where
        visited' = p `S.insert` visited
    walk trapped water start = fst . runWriterT $ bfsM next final start
      where
        next p = (neighbours p `prunning` (`S.member` psSet)

water ps = walk S.empty $ border vl vh
    where
    (vl, vh) --, rangeSize)
      = range ps
    walk visited open = -- traceShow visited $
      case open of
        Empty -> visited
        (p :<| open') ->
          neighbours p
          |> filter (\p
              -> inRange p
              && p `S.notMember` visited
              && p `S.notMember` ps)
          |> fromList
          |> (>< open')
          |> walk (p `S.insert` visited)

    inRange p = vl `cmpVec` p && p `cmpVec` vh
    cmpVec v1 v2 = and $ zipWith (<) (toList v1) (toList v2)

-- range :: Set V -> (V,V,Int)
range ps = (vl, vh) -- , rangeSize)
  where
    vecFromList [x,y,z] = V3 x y z
    mnmx ps lens = ps
      |> S.map (\v -> v ^. lens)
      |> (minimum &&& maximum)
    rs = map (mnmx ps) [_x, _y, _z]
    vl@(V3 xl yl zl) = map fst rs |> vecFromList
    vh@(V3 xh yh zh) = map snd rs |> vecFromList
    rangeSize = 0 -- (xh - xl + 1)

surface ps = foldl' f (S.size ps * 6)  ps
   where
     f n p =  countNeighboursInSet p ps |> (n -)

cube (V3 xl yl zl) (V3 xh yh zh) = fromList
  [V3 x y z
    | x <- [xl..xh]
    , y <- [yl..yh]
    , z <- [zl..zh]
    ]

border (V3 xl yl zl) (V3 xh yh zh) = fromList
  [V3 x y z
    | x <- [xl-1..xh+1]
    , y <- [yl-1..yh+1]
    , z <- [zl-1..zh+1]
    , x `elem` [xl-1, xh+1]
      || y `elem` [yl-1, yh+1]
      || z `elem` [zl-1, zh+1]
    ]

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

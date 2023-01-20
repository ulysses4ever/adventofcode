#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra, linear, unordered-containers, lens
-}
{-# language LambdaCase #-}
{-# language PatternSynonyms #-}

import Flow ((.>), (|>))
import Data.List
import Data.Either
import Data.List.Extra (chunksOf, split)
import Data.Foldable.Extra (notNull)
import Control.Arrow ((***), (&&&))

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S

import Debug.Trace

import Linear.Vector
import Linear.V2

import Control.Lens.Operators ((^.))

type P = V2 Int
pattern P x y = V2 x y

type Seg = (Int,Int)
type KSegs = (Int, [Seg])
type M = M.HashMap Int [Seg]
type Rocks = (M,M)

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
-- part :: Int -> ??? -> Int
part n = modelSand .> S.size

type Set = S.HashSet P
type State = (Set, Set, Bool)

modelSand inp = -- trace (showPoints $ S.toList closed)
  closed
  where
    initial = P 500 0
    rs = structureRocks inp
    maxy = foldl' mxFold 0 $ concat inp
      where
        mxFold mx (Left (x, ys)) = max mx (maximum $ map snd ys)
        mxFold mx (Right (y, _)) = max y mx

    (_,closed,_mx ) = go (S.singleton initial, S.empty, False) initial

    isHalting (P _ y) = y == maxy
    go :: State -> P -> State
    go s@(discovered, closed, halt) current
      -- | traceShow current False = undefined
      | halt = s
      | isHalting current = (discovered, closed, True)
      | otherwise = (discovered'', closed'', halt')
          where
            ns = neighbors current
              |> filter ((`S.member` discovered) .> not)
              |> filter (not . collide rs)
            discovered' = discovered `S.union` (S.fromList ns)
            (discovered'', closed', halt') = foldl' go (discovered', closed, halt) ns
            closed''
              | halt' = closed'
              | otherwise = current `S.insert` closed'

neighbors p = (p +) <$>
  [ (P    0 1)
  , (P (-1) 1)
  , (P    1 1)
  ]

structureRocks inp = concat inp
    |> partitionEithers
    |> (make_maps *** make_maps)
  where
    make_maps = M.fromListWith (++)

collide :: Rocks -> P -> Bool
collide (mx,my) (P x y) = lkp mx x y || lkp my y x
  where
    lkp m a b = case m M.!? a of
      Just segs -> any (\(l,r) -> l <= b && b <= r) segs
      Nothing   -> False

-- Read one line of problem's input into something more structured
parseLine :: String -> [Either KSegs KSegs]
parseLine = split (`elem` ", ->")
  .> filter notNull
  .> chunksOf 2
  .> (\ws -> zip ws $ tail ws)
  .> map collect
  where
    collect ([sx1, sy1], [sx2, sy2])
      |x1 == x2 = Left (x1, pure (min y1 y2, max y1 y2))
      |y1 == y2 = Right (y1, pure (min x1 x2, max x1 x2))
      | otherwise = error "impossible: line is not axis aligned"
      where
        x1 = read sx1
        x2 = read sx2
        y1 = read sy1
        y2 = read sy2


{--------------------------------------------------------
--
-- Debug print
--
--------------------------------------------------------}
minMax2D ps = ((xmin, xmax), (ymin, ymax))
  where
    mnmx ps lens = ps
      |> map (\v -> v ^. lens)
      |> (minimum &&& maximum)
    mnmxVal@[(xmin, xmax), (ymin, ymax)] = map (mnmx ps) [_x, _y]

showPoints :: [P] -> String
showPoints [] = []
showPoints ps = unlines $ foldl'
    (\ls r ->
      ls ++ (pure $ foldl' (\cs c ->
        cs ++ pure (if V2 r c `S.member` psSet then '#' else '.'))
      [] [ymin..ymax]))
    [] [xmin..xmax]
  where
    ((xmin, xmax), (ymin, ymax)) = minMax2D ps
    psSet = S.fromList ps


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

-- Turn problem's full text into something more structured
-- parse :: String -> ???
parse =  lines .> map parseLine

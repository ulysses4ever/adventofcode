module Main where

import AoC

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List (foldl')
import Data.Char (isDigit)
import Control.Monad.Trans.Writer.CPS (runWriter, tell, censor)
import Algorithm.Search (dfsM, pruning)
import Data.Maybe (maybeToList)

type M = Map P Char
type C = (M, Int, Int)

solve :: [String] -> Int -> Int
solve inp = -- traceShow rs $
  \case
    1 -> 0 -- traceShow part1 0
    2 -> part2
  where
    c@(m, rmx, cmx) = parse inp
    rs = roots m
    part1 = dfsRoot c (head rs)
    part2 = 0

dfsRoot :: C -> P -> [P]
dfsRoot (m, rmx, cmx) r = traceShow (init r) res
  where
    res = concatMap myDfs (init r)
    myDfs :: P -> [P]
    myDfs init =
      let (_, res) = runWriter $ dfsM next (const False .> pure) init
      in res
    --next :: P -> [P]
    -- next :: _
    next = eps `pruning` outofbound `pruning` ((`M.lookup` m) .> \case
                                               Nothing -> True
                                               Just c  -> not $ isDigit c
                                           )
                    .> (\ps -> traceShow ps $ censor (ps++) (pure ps))
    init q@(P r c)
      = [ p
        | dr <- [-1..1]
        , dc <- [-1..1]
        , let r' = r+dr
        , let c' = c+dc
        , let p = P r' c'
        , p /= q
        , c <- maybeToList $ p `M.lookup` m
        , isDigit c
        ]
    eps (P r c) = [P r (c-1), P r (c+1)]
    outofbound (P r c)
      =  not
      $  0 <= r && r < rmx
      && 0 <= c && c < cmx

roots :: M -> [P]
roots = M.foldrWithKey f []
  where
    f p c rs
      | not $ isDigit c = p : rs
      | otherwise = rs

parse :: [String] -> C
parse inp = (M.fromList psAll, length inp, length $ head inp)
  where
    psAll = foldl' fr [] (zip [0..] inp)

    fr ps (r, line) = foldl' fc ps (zip [0..] line)
      where
        fc ps (c, x)
            | x == '.' = ps
            | otherwise = (P r c, x) : ps

main :: IO ()
main = defaultMain solve

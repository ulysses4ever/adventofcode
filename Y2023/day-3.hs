module Main where

import AoC

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List (groupBy, foldl', sort)
import Data.Containers.ListUtils (nubOrd)
import Data.Char (isDigit)
import Control.Monad.Trans.Writer.CPS (runWriter, tell, censor)
import Algorithm.Search (dfsM, pruning)
import Data.Maybe (maybeToList, fromJust)

type M = Map P Char
type C = (M, Int, Int)

solve :: [String] -> Int -> Int
solve inp = -- traceShow rs $
  \case
    1 -> part1 -- traceShow nums (traceShow digs part1)
    2 -> part2
  where
    c@(m, rmx, cmx) = parse inp
    rs = roots m
    digs = sort $ nubOrd $ foldl' f [] rs
      where
        f ds p = ds ++ dfsRoot c p
    nums :: [Int]
    nums
      =  digs
      |> groupAdj
      .> map (map ((`M.lookup` m) .> fromJust) .> read)
    part1 = sum nums
    part2 = 0

groupAdj (p:ps) = go [[p]] ps
  where
    go (xs:xss) [] = reverse xs : xss
    go (xs@(q@(P rp cp) : qs):xss) (p@(P r c):ps)
      | r == rp && cp + 1 == c
         = go ((p:xs):xss) ps
      | otherwise
         = go ([p]:reverse xs:xss) ps

dfsRoot :: C -> P -> [P]
dfsRoot c@(m,rmx,cmx) r = res -- traceShow (init r) res
  where
    res = nubOrd $ concatMap myDfs (init r)
    myDfs :: P -> [P]
    myDfs init =
      let (_, res) = runWriter $ dfsM (next c) (const False .> pure) init
      in init : res
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
        ] |> filter (outofbound rmx cmx .> not)

next (m,rmx,cmx) = eps
  `pruning` outofbound rmx cmx
  `pruning` ((`M.lookup` m) .> \case
                                            Nothing -> True
                                            Just c  -> not $ isDigit c
                                        )
                .> (\ps -> censor (ps++) (pure ps))
  where
    eps (P r c) = [P r (c-1), P r (c+1)]
outofbound rmx cmx (P r c)
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

tiny =
  [   ".3....."
    , ".*12..."
    , ".34...."]
c=parse tiny

main :: IO ()
main =
  -- print $ dfsRoot c (P 1 1)
  -- runWriter $ dfsM (next c) (const False .> pure) (P 1 1) --
  defaultMain solve

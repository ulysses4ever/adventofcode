module Main where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (find)
import Data.Char
import AoC

solve :: Groups (Lines (Words String)) -> Int -> Int
solve (Gs [_, (Ls ls')]) = \case
    1 -> part1
    2 -> part2
  where
    ls :: [[String]]
    ls = coerce ls'

    db :: [(String,[String])]
    db = flip map ls \[fst, _eq, snd, trd] ->
      (fst, [tail $ init snd, init trd])

    db1 = IntMap.fromList
      $ filter (\(a,b) -> a /= b)
      $ flip concatMap db \(f,[s,t]) ->
        [(c s, c f), (c t,c f)]

    part1 = --pTraceShow db1
      (iterate (db1 IntMap.!) (c "ZZZ")
      |> takeWhile (/= c "AAA")
      |> length)

    part2 = 0

step db needle = (flip find db \(f,st) ->
  needle `elem` st) |> (\(Just (f,_)) -> f)

c :: String -> Int
c s = zipWith (*) [2,3,5] (map i s) |> sum
  where
    i c = ord c - ord 'A'

main :: IO ()
main = defaultMain solve

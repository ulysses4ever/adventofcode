module Main where

import Data.List (find)
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

    part1 = iterate (step db) "ZZZ" |> takeWhile (/= "AAA") |> length

    part2 = 0

step db needle = (flip find db \(f,st) ->
  needle `elem` st) |> (\(Just (f,_)) -> f)

main :: IO ()
main = defaultMain solve

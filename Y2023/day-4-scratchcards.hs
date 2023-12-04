module Main where

import AoC

import Data.List (intersect)

solve :: [String] -> Int -> Int
solve inp = \case
    1 -> part1
    2 -> part2
  where
    wsmy' :: [([Int],[Int])]
    wsmy' = do
      wsmy <- inp
      let (ws, my) = span (/= '|') wsmy
      pure (tail $ input ws, input my)

    score (ws, my) = let
      exp = (length (ws `intersect` my) - 1)
      in if exp < 0 then 0 else 2^exp
    part1 = map score wsmy' |> sum -- (traceShowId $ map score (traceShowId wsmy'))
    part2 = 0

main :: IO ()
main = defaultMain solve

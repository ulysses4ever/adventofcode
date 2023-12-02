module Main where

import AoC

solve :: [String] -> Int -> Int
solve inp = \case
    1 -> part1
    2 -> part2
  where
    part1 = 0
    part2 = 0

main :: IO ()
main = defaultMain solve

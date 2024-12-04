module Main (main) where

import AoC

solve :: [String] -> Int -> Int
solve inp = \case
    1 -> 0
    2 -> 0
  where

main :: IO ()
main = defaultMain solve

module Main where

import Data.Char

import AoC

solve :: [String] -> Int -> Int
solve inp = \case
    1 -> part1 inp
    2 -> 0
  where
    part1 = map getDigit .> sum
    getDigit :: String -> Int
    getDigit
      =  filter isDigit
      .> (\ds -> [head ds, last ds])
      .> (\i -> trace i $ read i)

main :: IO ()
main = defaultMain solve

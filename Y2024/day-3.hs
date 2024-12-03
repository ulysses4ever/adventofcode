module Main where

import AoC
import Text.Regex.TDFA

solve :: String -> Int -> Int
solve inp = \case
    1 -> map tail ms |> map (map read .> \[x, y] -> x * y) |> sum :: Int
    2 -> 0
  where
    ms = (inp =~ "mul\\(([0-9]*),([0-9]*)\\)") :: [[String]]

main :: IO ()
main = defaultMain solve

module Main where

import AoC
import Data.List (find)

solve :: [String] -> Int -> Int
solve inp = \case
    1 ->
      inp' |> map ext .> sum
    2 ->
      inp' |> map (reverse .> ext) .> sum
  where
    inp' = map (words .> map read) inp

ext :: [Int] -> Int
ext xs
  | all (==0) xs = 0
  | otherwise = last xs + ext (zipWith (-) (tail xs) xs)

main :: IO ()
main = defaultMain solve

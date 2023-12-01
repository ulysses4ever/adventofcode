module Main where

import AoC

solve :: [Int] -> Int -> Int
solve inp = \case
    1 -> sum . map fuel $ inp
    2 -> sum . concatMap iterateFuel $ inp

fuel :: Int -> Int
fuel mass = mass `div` 3 - 2

iterateFuel :: Int -> [Int]
iterateFuel x = iterate fuel x |> tail |> takeWhile (> 0)

main :: IO ()
main = defaultMain solve

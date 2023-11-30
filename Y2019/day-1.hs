module Main where

import AoC

solve :: [Int] -> (Int, Int)
solve inp = (part 1, part 2)
  where
    part 1 = sum . map fuel $ inp
    part 2 = sum . concatMap iterateFuel $ inp

fuel :: Int -> Int
fuel mass = mass `div` 3 - 2

iterateFuel :: Int -> [Int]
iterateFuel x = iterate fuel x |> tail |> takeWhile (> 0)

main :: IO ()
main = defaultMain solve

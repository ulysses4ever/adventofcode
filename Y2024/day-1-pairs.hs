module Main where

import AoC
import Data.List (transpose, sort)

solve :: Lines [Int] -> Int -> Int
solve (Ls ws) = \case
    1 -> part1
    2 -> part2
  where
    [ls, rs] = transpose ws

    part1 = zipWith (\a b -> abs $ a - b) (sort ls) (sort rs) |> sum
    part2 = foldl' f 0 ls

    f sum x = x * (filter (==x) rs |> length) + sum

main :: IO ()
main = defaultMain solve

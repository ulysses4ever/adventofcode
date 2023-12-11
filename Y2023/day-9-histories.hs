module Main where

import AoC

solve :: [[Int]] -> Int -> Int
solve inp = \case
    1 -> part1 -- pTraceShowCompact inp part1
    2 -> part2
  where
    part1 = inp |> map (histories .> pTraceShowIdCompact .> map last .> sum) .> sum
    part2 = 0

histories :: [Int] -> [[Int]]
histories xs
  =  iterate diff xs
  |> takeWhile (any (/= 0))

diff xs = foldr f [] (zip xs $ tail xs)
    where
    f (x, x') rs = let y=x'-x in (y:rs)

main :: IO ()
main = defaultMain solve

inp' :: [[Int]]
inp' =
  [ [ 0, 3, 6, 9, 12, 15 ], [ 1, 3, 6, 10, 15, 21 ], [ 10, 13, 16, 21, 30, 45 ] ]

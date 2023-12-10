module Main where

import AoC

solve :: [[Int]] -> Int -> Int
solve inp = \case
    1 -> pTraceShowCompact inp part1
    2 -> part2
  where
    part1 = 0 -- map extrapolate inp
    part2 = 0

extrapolate :: [Int] -> b
extrapolate = undefined

diff xs = foldr f (False,[]) (zip xs $ tail xs)
    where
    f (x, x') (nonZero, rs) = let y=x'-x in (nonZero || y/=0, y:rs)

main :: IO ()
main = defaultMain solve

inp' :: [[Int]]
inp' =
  [ [ 0, 3, 6, 9, 12, 15 ], [ 1, 3, 6, 10, 15, 21 ], [ 10, 13, 16, 21, 30, 45 ] ]

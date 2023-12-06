module Main where

import AoC

solve :: [[Int]] -> Int -> Int
solve inp@[ts,rs] = \case
    1 -> part1
    2 -> part2
  where
    ps = zip ts rs
    nrs = map (uncurry myRecords) ps
    part1 = product $ map length nrs
    part2 = 0

myRecords :: Int -> Int -> [Int]
myRecords t or
  = [ nr
    | b <- [1..t-1]
    , let nr = b * (t-b)
    , or < nr
    ]

main :: IO ()
main = defaultMain solve

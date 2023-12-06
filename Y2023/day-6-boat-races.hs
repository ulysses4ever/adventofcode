module Main where

import AoC
import Data.Char (isDigit)

solve :: String -> Int -> Int
solve inp = \case
    1 -> part1
    2 -> part2
  where
    ts,rs :: [Int]
    [ts,rs] = input inp
    ps = zip ts rs
    nrs = map (uncurry myRecords)
    part1 = product $ map length (nrs ps)

    t2, r2 :: String
    [t2,r2] = input inp
    ds = filter isDigit
    part2 = length . head $ nrs [(read $ ds t2, read $ ds r2)]

myRecords :: Int -> Int -> [Int]
myRecords t or
  = [ nr
    | b <- [1..t-1]
    , let nr = b * (t-b)
    , or < nr
    ]

main :: IO ()
main = defaultMain solve

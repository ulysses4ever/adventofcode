module Main where

import Data.List (sort, group)

import AoC
import Data.Char (isDigit, ord)

data C =
    C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | T | J | Q | K | A
    deriving (Eq, Ord, Bounded, Enum, Show, Read)

readCard :: Char -> C
readCard c
  | isDigit c = iterate succ C2 !! (ord c - ord '2')
  | otherwise = read [c]

solve :: [String] -> Int -> Int
solve inp = \case
    1 -> part1
    2 -> part2
  where
    hsbs :: [([C], Int)]
    hsbs = map (words .> \[h, b] -> (map readCard h, read b)) inp
    part1 = pTraceShowCompact hsbs 0
    part2 = 0

main :: IO ()
main = defaultMain solve

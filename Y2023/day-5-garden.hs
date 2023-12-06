module Main where

import AoC
import Data.List.Extra (sortOn, scanl')

type Map = [P3]

solve :: Groups (Lines [Int]) -> Int -> Int
solve inp@(Gs (Ls [seeds]:gs)) = \case
    1 -> part1 -- pTraceShowCompact maps (pTraceShowCompact planted part1)
    2 -> part2
  where

    maps :: [Map]
    maps = map (coerce .> tail .> sortOn (tail .> head) .> (\[a,b,c]->P3 a b c)) gs

    planted = map (seedAll maps) seeds

    part1 = planted |> minimum
    part2 = 0


seedAll :: [Map] -> Int -> Int
seedAll ms sd = last $ seedTrace ms sd

seedTrace :: [Map] -> Int -> [Int]
seedTrace ms sd = scanl' seed sd ms

seed :: Int -> Map -> Int
seed sd mp@([_,s1,_]:_)
  | sd < s1 = sd
  | otherwise =
    case dropWhile (\(P3 _ s r) -> s+r<=sd) mp of
        [] -> sd
        ((P3 d s _):_) -> sd - s + d

-- smallInp :: [[[Int]]]
-- smallInp =
--     [
--         [ [ 52, 50, 48 ], [ 50, 98, 2 ] ],
--         [ [ 39, 0, 15 ], [ 0, 15, 37 ], [ 37, 52, 2 ] ],
--         [ [ 42, 0, 7 ], [ 57, 7, 4 ], [ 0, 11, 42 ], [ 49, 53, 8 ] ],
--         [ [ 88, 18, 7 ], [ 18, 25, 70 ] ],
--         [ [ 81, 45, 19 ], [ 68, 64, 13 ], [ 45, 77, 23 ] ],
--         [ [ 1, 0, 69 ], [ 0, 69, 1 ] ],
--         [ [ 60, 56, 37 ], [ 56, 93, 4 ] ]
--     ]

main :: IO ()
main = defaultMain solve

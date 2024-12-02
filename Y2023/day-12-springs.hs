module Main where

import AoC

import Control.Exception (assert)
import Data.List (sort)

solve :: [String] -> Int -> Int
solve inp = \case
    1 -> part1
    2 -> part2
  where
    part1 = 0
    part2 = 0

tests = map sort [
    -- f [] "?",
    -- f [1] "?",
    -- f [1] "#",
    -- f [1] "??",
    -- f [1] "?.?",
    -- f [1] "?.??",
    -- f [2] "?#",
    f [1,1] "?.?"
    -- f [1,1,3] "???.###"
    ]
expected = map sort [
    -- ["."],
    -- ["#"],
    -- ["#"],
    -- ["#.", ".#"],
    -- ["#..", "..#"],
    -- ["#...", "..#.", "...#"],
    ["#.#"]
    -- ["#.#.###"]
    ]
runAllTests = assert (tests == expected) ()
f :: [Int] -> String -> [String]
-- f ns ss | traceShow (ns,ss) False = undefined
f [] ss =
  case ss of
    [] -> [[]]
    _  -> f [0] ss
f ns [] =
  case ns of
    [0] -> [[]]
    _   -> [] -- deadend: no place but some springs to place left
f ns@(x:xs) ts@(s:ss) = case s of
  '.' -> map ('.' :) $ f ns ss
  '#' -> map ('#' :) $ f (x-1:xs) ss
  '?' ->
    case x of
      0 -> map ('.' :) $ f xs ss
      _ -> map ('.' :) (f ns ss) ++
        map ('#' :) (f (x-1:xs) ss)

    -- (s':_) -> case s' of -- lookahead
    --   '.' -> case x of
    --     0 -> case xs of
    --       [] -> map ('.' :) $ f ns ss
    --       (x':_) -> case x' of
    --         0 -> map ('.' :) $ f ns ss

main :: IO ()
main = defaultMain solve

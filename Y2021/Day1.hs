{-# language BangPatterns #-}
module Y2021.Day1 (solve1, solve2) where

import Data.List

solve1 :: String -> IO ()
solve1 input = print $ countInc 1 $ parse input

parse :: String -> [Int]
parse input = map read $ lines input

countInc :: Int -> [Int] -> Int
countInc n xs = foldl' 
  (\s (x, x') -> if x < x' then s + 1 else s) 
  0
  (zip xs $ drop n xs)

solve2 :: String -> IO ()
solve2 input = print $ countInc 3 $ parse input


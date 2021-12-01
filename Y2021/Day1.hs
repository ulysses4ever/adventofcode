{-# language BangPatterns #-}
module Y2021.Day1 (solve1, solve2) where

import Data.List

solve1 :: String -> IO ()
solve1 input = print $ countInc (map read $ lines input)

countInc :: [Int] -> Int
countInc xs = foldl' 
  (\s (x, x') -> if x < x' then s + 1 else s) 
  0
  (zip xs $ tail xs)

solve2 :: String -> IO ()
solve2 _input = print ()


module Main where

import AoC
import Data.List (transpose, sort)
import qualified Data.IntMap.Strict as IntMap

solve :: [[Int]] -> Int -> Int
solve rows = \case
    1 -> zipWith (\a b -> abs $ a - b) (sort ls) (sort rs) |> sum
    2 -> map f ls |> sum
  where
    _cols@[ls, rs] = transpose rows
    crs = counter rs
    f x = x * IntMap.findWithDefault 0 x crs

main :: IO ()
main = defaultMain solve

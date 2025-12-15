module Main where

import AoC
import Data.List (transpose, sort)

solve :: [String] -> Int -> Int
solve inp _n = foldl' step (50, 0) inp |> snd
  where
    step (acc, res) (d:n) =
      let
        n' = read n
        acc' = case d of
                 'L' -> (acc - n' + 100) `mod` 100
                 'R' -> (acc + n') `mod` 100
      in (acc', upd acc' res)

    upd 0 res = res + 1
    upd _ res = res

main :: IO ()
main = defaultMain solve

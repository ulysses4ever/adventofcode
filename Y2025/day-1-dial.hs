module Main where

import AoC
import Data.List (transpose, sort)

solve :: [String] -> Int -> Int
solve inp = \case
  1 -> foldRes step1
  2 -> foldRes step2
  where
    foldRes f = foldl' f (50, 0) inp |> snd

    step1 (acc, res) (d:n) =
      let
        n' = read n
        acc' = case d of
                 'L' -> (acc - n' + 100) `mod` 100
                 'R' -> (acc + n') `mod` 100
      in (acc', upd acc' res)

    upd 0 res = res + 1
    upd _ res = res

    step2 (acc, res) (d:n) =
      let
        n' = read n
        acc' = case d of
                 'L' -> (acc - n')
                 'R' -> (acc + n')
        (dv, md) = acc' `divMod` 100
      in (md, res + abs dv)

main :: IO ()
main = defaultMain solve

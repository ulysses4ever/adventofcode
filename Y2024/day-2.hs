module Main where

import AoC
import Data.Maybe

solve :: [[Int]] -> Int -> Int
solve inp = \case
    1 -> countIf issafe inp
    2 -> countIf issafe' inp

diffokAll r = zipWith diffok r (tail r)
  where
  diffok x y = abs (x - y) <= 3

issafe r = and (diffokAll r)
    && ismono r

ismono r =
    and (mono' (>) r) ||
    and (mono' (<) r)
  where
    mono' pred r = zipWith pred r (tail r)

-- Part 2

-- Nothing if increasing, otherwise the index of the first element that breaks it minus one
mono2 r = zipWith (<) r (tail r)
    |> flip zip [0..]
    |> lookup False

issafe' r =
  almostsafe r || almostsafe (reverse r)

almostsafe r =
    case mono2 r of
        Nothing ->
          let dok = diffokAll r in
            and (tail dok) || and (init dok)
        Just n ->
          issafe (remove n r) || issafe (remove (n+1) r)

-- brute force -- works like a charm!
issafe'' r = issafe r || or [ issafe (remove n r) | n <- [0..(length r - 1)]]

main :: IO ()
main = defaultMain solve

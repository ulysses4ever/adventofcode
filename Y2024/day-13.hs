module Main (main) where

import AoC
import Math.NumberTheory.Euclidean (extendedGcd)

solve :: Groups [Int] -> Int -> Int
solve (Gs inp) = \case
    1 -> pTraceShowCompact inp 0
    2 -> 0
  where

totalcost [ax, ay, bx, by, x, y] = liftA2 (+) costx costy
  where
    costx = cost ax bx x
    costy = cost ay by y

cost a b c
  | m == 0 = undefined
  where
    (g, s, t) = extendedGcd a b
    (d, m) = g `divMod` c

main :: IO ()
main = defaultMain solve

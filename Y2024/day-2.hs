module Main where

import AoC

solve :: [[Int]] -> Int -> Int
solve inp = \case
    1 -> res issafe
    2 -> res issafe'
  where

    res safe = pTraceShowCompact (map safe inp) (filter safe inp |> length)

    diffok' r = zipWith diffok r (tail r)
    diffok x y = abs (x - y) <= 3

    issafe r = and (diffok' r)
        && ismono r

    mono' pred r = zipWith pred r (tail r)
    ismono r =
        and (mono' (>) r) ||
        and (mono' (<) r)

    almostok xs = (filter id xs |> length) - 1 >= length xs

    issafe' r = let
      inc = mono' (<) r
      dec = mono' (>) r
      -- incok = almostok inc
      -- decok = almostok dec
      diffok'' = diffok' r
      -- diffok'' = almostok diffok
      in -- (incok || decok) && diffok'' &&
         (align inc diffok'' || align dec diffok'')
    align xs ys = zipWith (&&) xs ys
      |> filter id
      |> length
      |> ((length xs - 1) <=)

main :: IO ()
main = defaultMain solve

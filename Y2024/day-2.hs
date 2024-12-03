module Main where

import AoC
import Data.Maybe

solve :: [[Int]] -> Int -> Int
solve inp = \case
    1 -> res issafe
    2 -> res issafe'
  where
    res safe = pTraceShowCompact (map safe inp)
      (filter safe inp |> length)

diffok' r = zipWith diffok r (tail r)
diffok x y = abs (x - y) <= 3

issafe r = and (diffok' r)
    && ismono r

ismono r =
    and (mono' (>) r) ||
    and (mono' (<) r)
  where
    mono' pred r = zipWith pred r (tail r)

-- Part 2

mono2 inc r = zipWith (monopred inc) r (tail r)
    |> flip zip [1..]
    |> lookup False
  where
    monopred inc x y = inc * (x - y) > 0

issafe' r = trace ("Is r=" ++ show r ++ " almost safe?")
  almostsafe 1 r || almostsafe -1 r

almostsafe inc r =
    case mono2 inc r of
        Nothing -> trace "  - It's already mono. Is diffok?"
          (pTraceShowIdCompact $ and $ diffok' r)
        Just n -> trace ("  - Not mono "
                         ++ (if inc == 1 then "one" else "another")
                         ++ " way, try to drop "
                         ++ show n ++ " and check safety: " )
          pTraceShowIdCompact (issafe (drop n r))

--t = pTraceShowCompact

main :: IO ()
main = defaultMain solve

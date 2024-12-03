module Main where

import AoC
import Text.Regex.TDFA

solve :: String -> Int -> Int
solve inp = \case
    1 -> ms |> map eval |> sum
    2 -> ms'' |> map eval |> sum -- pTraceShowCompact gs 0
  where
    -- Part 1
    eval :: [String] -> Int
    eval = tail .> map read .> \[x, y] -> x * y
    p1re = "mul\\(([0-9]*),([0-9]*)\\)"
    ms = (inp =~ p1re) :: [[String]]

    -- Part 2
    ms' = (inp =~ ("" ++ p1re ++ "|do\\(\\)|don't\\(\\)")) :: [[String]]
    gs = groupOn
      (\cmd -> cmd == doCmd
            || cmd == dontCmd
      ) (doCmd : ms')
    ms'' = zipWith f gs (tail gs) |> catMaybes |> concat
      where
        f d m
          | d == [doCmd] = Just m
          | otherwise  = Nothing

doCmd = ["do()","",""]
dontCmd = ["don't()","",""]

main :: IO ()
main = defaultMain solve

module Main where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict ((!))
import Data.List (find)
import Data.Char
import AoC

solve :: (String, Lines (Words String)) -> Int -> Int
solve (cs, (Ls ls')) = \case
    1 -> part1
    2 -> part2
  where
    ls :: [[String]]
    ls = coerce ls'

    db :: IntMap (Int,Int,String)
    db = IntMap.fromList $
      flip map ls \[fst, _eq, snd, trd] ->
        (c fst, (c $ tail $ init snd, c $ init trd, fst))

    part1 =
      interp db (cycle cs) 0 (c "AAA")

    part2 = 0

zzz = c "ZZZ"

interp db (c:cs) index state
  | state == zzz = index
  | c == 'L' = next l
  | c == 'R' = next r
  where
    (l, r, lbl) = db ! state
    next = interp db cs (index+1)

step db needle = (flip find db \(f,st) ->
  needle `elem` st) |> (\(Just (f,_)) -> f)

c :: String -> Int
c s = zipWith (*) [1,26,26*26] (map i s) |> sum
  where
    i c = ord c - ord 'A'

main :: IO ()
main = defaultMain solve
